(ns fsm-abonnement.core)


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

(let [char-seq (ref "ablislasllllispsslis")
       sm (state-machine (find-lisp char-seq) :start)] 
   (dotimes [_ (count @char-seq)]
     (update-state sm)))