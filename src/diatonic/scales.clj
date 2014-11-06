(ns diatonic.scales)

(defprotocol Scale
  (base-pitch [scale])
  (steps [scale])
  (get-step [scale step])
  (diatonic-steps [scale])
  (chord-steps [scale])
  (derived-scale [scale step]))

(def ionic-scale-steps
  [{:step 0 :diatonic true :chord true :base true}
   {:step 1}
   {:step 2 :diatonic true}
   {:step 3}
   {:step 4 :diatonic true :chord true :third true}
   {:step 5 :diatonic true :avoid true}
   {:step 6}
   {:step 7 :diatonic true :chord true :fifth true}
   {:step 8}
   {:step 9 :diatonic true}
   {:step 10}
   {:step 11 :diatonic true :seventh true}])

(def aeolic-scale-steps
  [{:step 0 :diatonic true :chord true :base true}
   {:step 1}
   {:step 2 :diatonic true}
   {:step 3 :diatonic true :chord true :third true}
   {:step 4}
   {:step 5 :diatonic true}
   {:step 6}
   {:step 7 :diatonic true :chord true :fifth true}
   {:step 8 :diatonic true}
   {:step 9}
   {:step 10 :diatonic true :seventh true}
   {:step 11}])

(defn- split-and-recombine-steps [steps step]
  (let [[front back] (split-at step steps)
        recombined (concat back front)]
    (mapv (fn [step index]
           (-> step
               (assoc :step index)
               (dissoc :chord :base :third :fifth :seventh)))
         recombined (range))))

(defn- recompute-attributes [steps]
  (let [diatonics (filter :diatonic steps)
        base (first diatonics)
        third (nth diatonics 2)
        fifth (nth diatonics 4)
        seventh (last diatonics)]
    (-> steps
        (assoc-in [(:step base) :base] true)
        (assoc-in [(:step base) :chord] true)
        (assoc-in [(:step third) :third] true)
        (assoc-in [(:step third) :chord] true)
        (assoc-in [(:step fifth) :base] true)
        (assoc-in [(:step fifth) :chord] true)
        (assoc-in [(:step seventh) :seventh] true))))


(deftype SteppedScale [base scale-steps]
  Scale
  (base-pitch [scale] base)
  (steps [scale] scale-steps)
  (get-step [scale step] (get (steps scale) (mod step 12)))
  (diatonic-steps [scale] (filter :diatonic scale-steps))
  (chord-steps [scale] (filter :chord scale-steps))
  (derived-scale [scale step]
    (let [normalized-step (mod step 12)]
      (if (= normalized-step 0)
        scale
        (let [new-steps (-> (steps scale)
                            (split-and-recombine-steps normalized-step)
                            (recompute-attributes))]
          (SteppedScale. (:pitch (first new-steps)) new-steps))))))

(defn- scale [base-steps base-pitch]
  (SteppedScale. base-pitch
                 (mapv #(assoc % :pitch (+ base-pitch (:step %)))
                       base-steps)))

(def ionic-scale (partial scale ionic-scale-steps))
(def aeolic-scale (partial scale aeolic-scale-steps))
