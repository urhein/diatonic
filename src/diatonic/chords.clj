(ns diatonic.chords
  (:require [clojure.data.generators :as g]
            [diatonic.notes :as n]
            [diatonic.scales :as s]))

(defn- step-weight [step]
  (+ 3 (if (:base step) 1 0) (if (:chord step) 1 0)))

(defn chords [base-scale len]
  "Returns a seq of len scales, starting with base-scale."
  (let [diatonic-steps (s/diatonic-steps base-scale)
        step-weights (into {} (map (juxt identity step-weight)
                                   diatonic-steps))
        chord-fn (fn []
                   (let [scale-step (g/weighted step-weights)]
                     (s/derived-scale base-scale (:step scale-step))))]
    (cons base-scale (take (dec len) (repeatedly chord-fn)))))

(defn- inversion [chord]
  (let [scale (:chord chord)
        base-step (:base-step chord)
        diatonics (s/diatonic-steps scale)
        base (first diatonics)
        third (nth diatonics 2)
        fifth (nth diatonics 4)]
    (condp = base-step
      0 [base third fifth]
      2 [(n/transpose -12 fifth) base third]
      4 [third fifth (n/transpose 12 base)])))

(defn- place-chord-near
  "Place chord (specified by a note vector) near to the specified
  base pitch, by eventually transposing the notes by octaves."
  [base-pitch chord]
  (let [avg-pitch (int (/ (apply + (map :pitch chord)) (count chord)))
        pitch-diff (- base-pitch avg-pitch)
        pitch-rem (rem pitch-diff 12)
        pitch-adjust (if (> (Math/abs pitch-rem) 6)
                       (if (>= pitch-rem 0) -12 12) 0)
        transpose-by (- pitch-diff pitch-rem pitch-adjust)]
    (map (partial n/transpose transpose-by) chord)))

(defn- chord-notes [chord base-scale velocity offset length]
  (map #(assoc % :velocity velocity :offset offset :length length)
       (place-chord-near (s/base-pitch base-scale) (inversion chord))))

(defn init-chords
  "Initializes the chord list of the specified context, if it's not
  already present."
  [context]
  (if-not (:chords context)
    (let [base-scale (:base-scale context)
          chords (chords base-scale (:bars context))
          inversion-weights {0 5, 2 1, 4 1}]
      (assoc context :chords
             (vec
              (map-indexed
               (fn [idx chord]
                 {:chord chord
                  :base-step (if (> idx 0)
                               (g/weighted inversion-weights) 0)})
               chords))))
    context))

(defn init-chord-notes
  "Initializes the chord note list of the context, if it's not already
  present."
  [context]
  (if-not (:chord-notes context)
    (let [base-scale (:base-scale context)
          chords (:chords context)
          chord-len (:notes-per-bar context)]
      (assoc context :chord-notes
             (mapcat
              (fn [chord offset]
                (chord-notes chord base-scale 120 offset chord-len))
              chords (range 0 (* (:bars context) chord-len) chord-len))))
    context))
