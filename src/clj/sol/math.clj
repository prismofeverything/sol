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
