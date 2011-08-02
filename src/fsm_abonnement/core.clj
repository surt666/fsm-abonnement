(ns fsm-abonnement.core
  (:use [clojure.contrib.seq-utils :only [find-first]]))

(defn state-machine [transition-table initial-state]
  (ref initial-state :meta transition-table))

(defn- switch-state? [conds]
   (if (empty? conds)
     true
     (not (some false? (reduce #(conj %1 (if (fn? %2) (%2) %2)) [] conds)))))

(defn- first-valid-transition [ts]
  (find-first #(= (second %) true)
              (map #(let [{conds :conditions 
                           transition :transition
                           on-success :on-success} %]
                      [transition (switch-state? conds) on-success]) ts)))

(defn update-state [state]
  (let [transition-list ((meta state) @state)
        [transition _ on-success] (first-valid-transition transition-list)]
    (if-not (nil? transition)
      (do 
        (if-not (nil? on-success)
          (on-success))
        (dosync (ref-set state transition))))))

(defn abon-state [new-state]
  "Bruges hvis status aendres i ARIA og der skal reprovisioneres"
  {:pending [{:conditions [#(= new-state :active)] :on-success #(prn "State fra pending til active") :transition :active}]
   :active [{:conditions [#(= new-state :terminated)] :on-success #(prn "State fra active til terminated") :transition :terminated}]
   :suspended [{:conditions [#(= new-state :active)] :on-success #(prn "State fra suspended til active") :transition :active}] 
   :canceled [{:conditions [#(= new-state :active)] :on-success #(prn "State fra canceled til active") :transition :active}]
   :cancellation-pending [{:conditions [#(= new-state :canceled)] :on-success #(prn "State fra cancellation-pending til canceled") :transition :canceled}]
   :terminated [{:conditions [#(= new-state :active)] :on-success #(prn "State fra terminated til active") :transition :active}]
   :permanent [{:conditions [#(= new-state :terminated)] :on-success #(prn "State fra permanent til terminated") :transition :terminated}]
   :inactive [{:conditions [#(= new-state :active)] :on-success #(prn "State fra inactive til active") :transition :active}]})

(defn abon-input [input]
  "Bruges hvis der sker en handling i ordre systemet der skal medfoere status aendring samt evt. lager bestilling og/eller provisionering"
  {:start [{:conditions [#(= input :opret)] :on-success #(prn "Opret pending") :transition :pending}]
   :pending [{:conditions [#(= input :itakst)] :on-success #(prn "Saet Itakst") :transition :active}
             {:conditions [#(= input :itakst-vip)] :on-success #(prn "Saet til VIP") :transition :permanent}]
   :active [{:conditions [#(= input :terminated)] :on-success #(prn "Termineret") :transition :terminated}
            {:conditions [#(= input :canceled)] :on-success #(prn "Ophoer") :transition :cancellation-pending}
            {:conditions [#(= input :skift-produkt)] :on-success #(prn "Reprovisioner abonnement") :transition :active}
            {:conditions [#(= input :manglende-betaling)] :on-success #(prn "Suspender abonnement pga. manglende betaling") :transition :suspended}]
   :suspended [{:conditions [#(= input :reaktiver)] :on-success #(prn "Reaktiveret fra suspended") :transition :active}] 
   :canceled [{:conditions [#(= input :reaktiver)] :on-success #(prn "Reaktiveret fra lukning") :transition :active}]
   :cancellation-pending [{:conditions [#(= input :cancellation-date)] :on-success #(prn "Lukket") :transition :canceled}]
   :terminated [{:conditions [#(= input :reaktiver)] :on-success #(prn "Reaktiveret fra terminering") :transition :active}]
   :permanent [{:conditions [#(= input :terminated)] :on-success #(prn "Termineret VIP") :transition :terminated}]
   :inactive [{:conditions [#(= input :reaktiver)] :on-success #(prn "Reaktiver fra inaktiv") :transition :active}]})

(let [sm (state-machine (abon-state :canceled) :cancellation-pending)]    
  (update-state sm)
  (println @sm))

(def traffic-light
     {:green [{:conditions [] :transition :yellow}]
      :yellow  [{:conditions [] :transition :red}]
      :red [{:conditions [] :transition :green}]})

(comment (let [sm (state-machine traffic-light :green)]
   (dotimes [_ 4]
     (println @sm)
     (update-state sm))))


(defn pop-char [char-seq]
  (dosync (ref-set char-seq (rest @char-seq))))

(defn find-lisp [char-seq]
   (let [start-trans {:conditions []
                      :on-success #(pop-char char-seq)
                      :transition :start}
         found-l-trans {:conditions [#(= (first @char-seq) \l)] 
                        :on-success #(pop-char char-seq)
                        :transition :found-l}]

     {:start [found-l-trans
              start-trans]

      :found-l [found-l-trans
                {:conditions [#(= (first @char-seq) \i)] 
                 :on-success #(pop-char char-seq)
                 :transition :found-i}
                start-trans]

      :found-i [found-l-trans
                {:conditions [#(= (first @char-seq) \s)] 
                 :on-success #(pop-char char-seq)
                 :transition :found-s}
                start-trans]

      :found-s [found-l-trans
                {:conditions [#(= (first @char-seq) \p)] 
                 :on-success #(do (println "Found Lisp")
                                  (pop-char char-seq))
                 :transition :start}
                start-trans]}))

(comment (let [char-seq (ref "ablisplasllllispsslis")
       sm (state-machine (find-lisp char-seq) :start)] 
   (dotimes [_ (count @char-seq)]
     (update-state sm))))