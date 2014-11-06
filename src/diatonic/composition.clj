(ns diatonic.composition
  (:require [clojure.data.generators :as g]
            [diatonic.scales :as s]
            [diatonic.midi :as m]))

(defmacro with-seed [seed & body]
  "Creates a PRNG with the specified seed and binds it to be used in the body."
  `(binding [g/*rnd* (java.util.Random. ~seed)]
     (do ~@body)))

(defn- step-weight [step]
  (+ 3 (if (:base step) 1 0) (if (:chord step) 1 0)))

(defn chords [base-scale len]
  (let [diatonic-steps (s/diatonic-steps base-scale)
        step-weights (into {} (map (juxt identity step-weight)
                                   diatonic-steps))
        chord-fn (fn []
                   (let [scale-step (g/weighted step-weights)]
                     (s/derived-scale base-scale (:step scale-step))))]
    (cons base-scale (take (dec len) (repeatedly chord-fn)))))

(defn chord-notes [scale velocity offset length]
  (let [diatonics (s/diatonic-steps scale)
        base (first diatonics)
        third (nth diatonics 2)
        fifth (nth diatonics 4)]
    (map #(assoc % :velocity velocity :offset offset :length length)
         [base third fifth])))

(defn chord-track [base-scale len chord-len]
  (let [c (chords base-scale len)]
    (mapcat (fn [chord offset] (chord-notes chord 127 offset chord-len))
            c (range 0 (* len chord-len) chord-len))))

; (m/play-sequence
;  (m/midi-sequence
;   (chord-track (s/ionic-scale 60) 4 32) 128))
