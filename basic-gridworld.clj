;; Boundaries for the grid
(def x-start 0)
(def x-end 4)

(def y-start 0)
(def y-end 4)

;; Helper functions
(defn vec-add [v-1 v-2]
  (mapv + v-1 v-2))


(defn within-grid? [state]
  (->> state
       (mapv #(<= x-start % x-end))
       (every? true?)))

(defn argmax [s]
  (first (apply max-key second (map-indexed vector s))))


;; We start at position [0 0], the top left cell
(def start [x-start y-start])
;; The goal is to end up at [4 4], the bottom right cell
(def goal [x-end y-end])
;; Possible actions for the agent to take
(def actions {:up [-1 0] :down [1 0] :right [0 1] :left [0 -1]})
;; Setting initial values
(def values (vec (for [_ (range 5)]
                      [0 0 0 0 0]))) 
;; Setting initial policy
(def policy (vec (for [_ (range 5)]
                      (vec (for [_ (range 5)] :up)))))

;; Crossing each grid cell with the actions the agent can take at that state
(def state-cross-actions
  (partitionv 4
    (vec (for [row-pos (range (inc y-end))
               col-pos (range (inc x-end))
               action (keys actions)]
           [[row-pos col-pos] action]))))

;; Main Code
(defn get-next-state [current-state action]
  (let [potential-next-state (vec-add current-state action)]
    (if (within-grid? potential-next-state)
        potential-next-state
        current-state)))

(defn get-reward [state action]
  (let [next-state (get-next-state state action)
        negative-feedback -1
        positive-feedback 0]
    (if (= next-state goal) positive-feedback negative-feedback)))


(defn iter [state-cross-actions values policy]
 (loop [state-cross-actions state-cross-actions
        values values
        policy policy]
   (if (empty? state-cross-actions)
      [values policy]
      (let [cell-actions (first state-cross-actions)
            state (first (first cell-actions))
            rewards (for [[state action] cell-actions] (+ (get-reward state (action actions))
                                                          (get-in values (get-next-state state (action actions)))))]
        (recur (rest state-cross-actions)
               (assoc-in values state (apply max rewards))
               (assoc-in policy state (get (vec (keys actions)) (argmax rewards))))))))


(loop [state-cross-actions state-cross-actions
       values values
       policy policy
       n 1000]
  (if (zero? n)
    [values policy] 
    (let [[values policy] (iter state-cross-actions values policy)]
     (recur
       state-cross-actions
       values
       policy
       (dec n)))))
