(ns diatonic.drums
  (:require [clojure.data.generators :as g]))

(defn- hi-hat-pattern []
  (let [skip (g/weighted {4 3 2 1})
        up-velocity 127
        low-velocity (g/uniform 90 127)]
    (map (fn [offset index]
           {:offset offset :pitch 42 :length 1
            :velocity (if (even? index) up-velocity low-velocity)})
         (range 0 32 skip) (range))))

(defn- snare-pattern []
  ; just the backbeat, for now
  [{:offset 8 :pitch 38 :length 1 :velocity 127}
   {:offset 24 :pitch 38 :length 1 :velocity 127}])

(defn- kick-pattern []
  ; just simple for now
  [{:offset 0 :pitch 36 :length 1 :velocity 127}
   {:offset 16 :pitch 36 :length 1 :velocity 127}])

(defn- drum-pattern []
  (concat (hi-hat-pattern) (snare-pattern) (kick-pattern)))

(defn- drum-notes [len]
  (let [pattern (drum-pattern)]
    (mapcat (fn [bar] (map #(update-in % [:offset] + (* bar 32)) pattern))
            (range 0 len))))

(defn drum-track [len]
  {:type :drums :notes (drum-notes len)})
