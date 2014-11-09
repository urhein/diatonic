(ns diatonic.composition
  (:require [clojure.data.generators :as g]
            [diatonic.chords :as c]
            [diatonic.drums :as d]
            [diatonic.midi :as m]
            [diatonic.scales :as s]))

(defmacro with-seed [seed & body]
  "Creates a PRNG with the specified seed and binds it to be used in
  the body."
  `(binding [g/*rnd* (java.util.Random. ~seed)]
     (do ~@body)))

(defn init-context
  "Initializes an empty context with default values."
  ([] (init-context {}))
  ([context] (merge {:notes-per-bar 32 :bars 4} context)))

(defn init-base-scale
  "Initializes the base scale of the specified context, if it's not
  already present."
  [context]
  (if (:base-scale context)
    context
    (assoc context :base-scale
           ((if (g/boolean) s/ionic-scale s/aeolic-scale) 64))))

(defn compose [seed]
  (with-seed (if (string? seed) (.hashCode seed) seed)
    (let [context (init-context {:seed seed})]
      (-> context
          (init-base-scale)
          (c/init-chords)
          (c/init-chord-notes)))))

; (compose "1234")
(defn play [seed]
  (with-seed (if (string? seed) (.hashCode seed) seed)
    (let [len 4
          base-scale (if (g/boolean) s/ionic-scale s/aeolic-scale)
          chords (c/chord-track (base-scale 64) len 32)
          drums (d/drum-track len)]
      (m/play-sequence
       (m/midi-sequence 128 [chords drums]))))
  seed)

; (play (g/long))
; (play "foobar")
