(ns diatonic.midi
  (:import
    [javax.sound.midi MidiEvent MidiSystem MetaMessage Sequence ShortMessage]))

(defonce sequencer (MidiSystem/getSequencer))

(defn- short-message [command channel data1 data2]
  (let [msg (ShortMessage.)]
    (.setMessage msg command channel data1 data2)
    msg))

(defn- midi-event [message tick]
  (MidiEvent. message tick))

(defn- tempo-event [bpm]
  (let [message (MetaMessage.)
        mpq (Math/round (/ 60000000.0 bpm))
        data (byte-array 3)]
    (aset-byte data 0 (bit-and (Byte/MAX_VALUE) (bit-shift-right mpq 16)))
    (aset-byte data 1 (bit-and (Byte/MAX_VALUE) (bit-shift-right mpq 8)))
    (aset-byte data 2 (bit-and (Byte/MAX_VALUE) mpq))
    (.setMessage message 0x51 data (alength data))
    (midi-event message 0)))

(defn- note-on-event [pitch velocity tick]
  (let [on-msg (short-message (ShortMessage/NOTE_ON), 0, pitch, velocity)]
    (midi-event on-msg tick)))

(defn- note-off-event [pitch tick]
  (let [off-msg (short-message (ShortMessage/NOTE_OFF), 0, pitch, 64)]
    (midi-event off-msg tick)))

(defn midi-sequence [bpm tracks]
  (let [sequence (Sequence. (Sequence/PPQ) 8)]
    (doseq [notes tracks]
      (let [track (.createTrack sequence)]
        (doseq [note notes]
          (doto track
            (.add (note-on-event (:pitch note)
                                 (:velocity note)
                                 (:offset note)))
            (.add (note-off-event (:pitch note)
                                  (+ (:offset note)
                                     (:length note))))))))
    (.add (first (.getTracks sequence)) (tempo-event bpm))
    sequence))

(defn play-sequence [sequence]
  (.start
   (Thread.
    (fn []
      (locking sequencer
        (with-open [s sequencer]
          (doto sequencer
            (.setSequence sequence)
            (.open)
            (.start))
          (loop []
            (when (.isRunning sequencer)
              (Thread/sleep 100)
              (recur)))
          (Thread/sleep 200)))))))
