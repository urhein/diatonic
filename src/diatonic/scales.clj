(ns diatonic.scales)

(defprotocol Scale
  (base-pitch [scale])
  (steps [scale])
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
