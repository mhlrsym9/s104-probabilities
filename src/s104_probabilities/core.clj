(ns s104-probabilities.core
  (:gen-class))

(defn DR []
  {:d1 (inc (rand-int 6)) :d2 (inc (rand-int 6))})

(defn off-map-morale-check []
  (let [{:keys [d1 d2]} (DR)]
    (if (< (+ d1 d2) 9)
      :land-fine-last-row
      :land-broken-last-row)))

(defn landing-roll []
  (let [{:keys [d1 d2]} (DR)]
    (cond (= d1 1) :land-fine-last-row
          (= d1 2) (off-map-morale-check)
          (= d1 3) (off-map-morale-check)
          (= d1 4) (off-map-morale-check)
          (= d1 5) (cond (= d2 1) (off-map-morale-check)
                         (= d2 2) (off-map-morale-check)
                         :else :land-not-last-row)
          (= d1 6) :land-not-last-row)))

(defn perform-landing []
  (repeatedly 8 #(landing-roll)))

(defn n-landings [landings n]
  (filter (fn [landing] (= n (count landing))) landings))

(defn -main
  "I don't do a whole lot ... yet."
  [n & args]
  (let [landings (repeatedly n #(perform-landing))
        only-fine-landings (map (fn [landing] (filter #(= :land-fine-last-row %) landing)) landings)
        successful-landings (filter (fn [landing] (>= (count landing) 3)) only-fine-landings)
        fnc (partial n-landings only-fine-landings)
        answers (map #(count (fnc %)) (range 0 9))]
    (println (str "Number of successful landings " (count successful-landings)))
    (println answers))
  (println ))
