(ns diatonic.composition
  (:require [clojure.data.generators :as g]
            [diatonic.drums :as d]
            [diatonic.notes :as n]
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

(defn- inversion [scale]
  (let [diatonics (s/diatonic-steps scale)
        base (first diatonics)
        third (nth diatonics 2)
        fifth (nth diatonics 4)
        type (g/weighted {:base 50 :inv-1 10 :inv-2 10})]
    (condp = type
      :base [base third fifth]
      :inv-1 [base third (n/transpose fifth -12)]
      :inv-2 [(n/transpose base 12) third fifth])))

(defn chord-notes [scale velocity offset length]
  (map #(assoc % :velocity velocity :offset offset :length length)
       (inversion scale)))

(defn chord-track [base-scale len chord-len]
  (let [c (chords base-scale len)]
    {:type :chord
     :notes (mapcat
             (fn [chord offset] (chord-notes chord 127 offset chord-len))
             c (range 0 (* len chord-len) chord-len))}))

(defn play [seed]
  (with-seed seed
    (let [len 4
          base-scale (if (g/boolean) s/ionic-scale s/aeolic-scale)
          chords (chord-track (base-scale 60) len 32)
          drums (d/drum-track len)]
      (m/play-sequence
       (m/midi-sequence 128 [chords drums]))))
  seed)

; (play (g/long))
