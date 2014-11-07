(ns diatonic.composition
  (:require [clojure.data.generators :as g]
            [diatonic.chords :as c]
            [diatonic.drums :as d]
            [diatonic.midi :as m]
            [diatonic.scales :as s]))

(defmacro with-seed [seed & body]
  "Creates a PRNG with the specified seed and binds it to be used in the body."
  `(binding [g/*rnd* (java.util.Random. ~seed)]
     (do ~@body)))


(defn play [seed]
  (with-seed seed
    (let [len 4
          base-scale (if (g/boolean) s/ionic-scale s/aeolic-scale)
          chords (c/chord-track (base-scale 64) len 32)
          drums (d/drum-track len)]
      (m/play-sequence
       (m/midi-sequence 128 [chords drums]))))
  seed)

; (play (g/long))
