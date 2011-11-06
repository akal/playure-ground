(ns maximumlikelyhood)

(defn add-vecs [vec-of-vecs]
  (reduce (fn [x y] (map + x y)) vec-of-vecs))

;mu
(defn mean [inp]
  "Returns a mean value if inp is a vector, or a mean vector if inp is vector of vectors"
  (let [divisor (/ 1 (count inp))]
    (if (vector? (first inp))
      (map #(* %1 divisor) (reduce (fn [v1 v2] (map + v1 v2)) inp))
      (* divisor (reduce + inp)))))
;(mean [1 2 3 4 5])
;(mean [ [1 2] [3 4] [5 6] [7 8] ] )


;sigma sqared
(defn variance [inp]
  (let [m (mean inp)]
    (* (/ 1 (count inp))
       (reduce + (map 
                   #(* (- % m) (- % m)) 
                   inp) ))))

(defn res [inp] (println (format "input: %s, mean: %.2f, variance: %.2f" inp (float (mean inp)) (float (variance inp)))))

(res [3 4 5 6 7])
(res [3 9 9 3])

