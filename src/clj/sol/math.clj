(ns sol.math)

(def twopi (* Math/PI 2))

(defn make-ngon
  [sides radius start-angle]
  (let [angle (/ twopi sides)
        radials (map (fn [n] (+ start-angle (* angle n))) (range sides))]
    (map 
     (fn [radial] 
       [(* radius (Math/sin radial)) 
        (* radius (Math/cos radial))]) 
     radials)))

(defn merge-sort
  [c]
  (let [n (count c)]
    (if (<= n 1)
      c
      (let [split (split-at (quot n 2) c)
            [a b] (map merge-sort split)]
        (loop [output []
               a a
               b b]
          (cond 
           (empty? a) (concat output b)
           (empty? b) (concat output a)
           :else
           (let [next-a (first a)
                 next-b (first b)]
             (if (<= next-a next-b)
               (recur (conj output next-a) (rest a) b)
               (recur (conj output next-b) a (rest b))))))))))

(defn make-graph
  []
  {:nodes {}
   :edges {}})

(defn add-node
  [graph id value]
  (assoc-in graph [:nodes id] value))

(defn add-edge
  [graph from to weight]
  (assoc-in graph [:edges from to] weight))

(defn neighbors-for
  [graph id]
  (keys (get-in graph [:edges id])))

(defn depth-first
  [graph start]
  (loop [here start
         stack (list)
         visited #{}
         order []]
    (let [unvisited (remove visited (neighbors-for graph here))
          stack (concat unvisited stack)]
      (if (empty? stack)
        (conj order here)
        (recur (first stack) (rest stack) (conj visited here) (conj order here))))))

(defn queue
  []
  (clojure.lang.PersistentQueue/EMPTY))

(defn n-alike?
  [n seq]
  (not (empty? (filter (partial = n) (map count (vals (group-by identity seq)))))))

(defn generate-combinations
  "all possibilities of rolling n dice with x sides"
  [n x]
  ())

