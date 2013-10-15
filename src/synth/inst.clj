(ns synth.inst
  (:use
   [clojure.pprint]
   [overtone.live]
   [overtone.inst.drum :only [quick-kick haziti-clap soft-hat open-hat]]
   [overtone.inst.sampled-piano]))



(def server (osc-server 44100 "osc-clj"))
(def client (osc-client "192.168.1.5" 9801))

(def handlers (atom {}))
(def channels (atom (vec (repeat 8 true))))
(def measure-count 8)
(def closer-probs (atom (vec (map #(/ %1 measure-count) (range (inc measure-count))))))
(def change-probs (atom (vec (repeat (inc measure-count) 8/10))))

(defn update-closer-probs [cost new-value]
  (swap! closer-probs (fn [pvec]
                        (assoc pvec cost new-value))))

(defn update-change-probs [_ new-value]
  (swap! change-probs (fn [pvec]
                        (vec (repeat (inc measure-count) new-value)))))
(defn pchange [cost]
  (get @change-probs cost))

(def beat-affinity (atom 0.0))

(defn pcloser [cost] (cond (zero? cost) 0
                           (>= cost measure-count) 1
                           :else (get @closer-probs cost)))
(defn pfurther [cost] (- 1 (pcloser cost)))

(defn next-beat-dir [cost]
  (if (> (rand) (pchange cost))
    :nochange
    (if (< (rand) (pcloser cost))
      :closer
      :further)))

(defn ppp [& o]
  (do (pprint o) o))


(defn cost-to-p [cost nbeats]
  (/ (Math/log (inc (* cost @beat-affinity)))
     (Math/log (inc (* nbeats @beat-affinity)))))

(defn beat-changes [beat1 beat2]
  (vec (map bit-xor beat1 beat2)))

(defn beat-distance
  ([changes] (reduce + changes))
  ([beat1 beat2] (beat-distance (beat-changes beat1 beat2))))

(defn count-scan [c v]
  (reduce #(let [l (or (last %1) -1)] (conj %1 (+ l (if (= %2 v) 1 0))))
          []
          c))

(defn random-index [s val]
  (second (reduce (fn [[c idx :as v] i]
                    (if (= val (get s i))
                      (let [new-c (inc c)]
                        [new-c (if (< (rand new-c) 1) i idx)])
                      v))
                  [0 nil]
                  (range (count s)))))

(defn beat-further [beat changes]
  (let [zeros (- (count changes) (beat-distance changes))]
    (if (zero? zeros) beat
        (update-in beat [(random-index changes 0)] bit-flip 0))))

(defn beat-closer [beat changes]
  (let [ones (beat-distance changes)]
    (if (zero? ones) beat
        (update-in beat [(random-index changes 1)] bit-flip 0))))

(defn new-beat [base-beat old-beat]
  (let [changes (beat-changes base-beat old-beat)
        cost (beat-distance changes)]
    (case (next-beat-dir cost)
      :nochange old-beat
      :closer (beat-closer old-beat changes)
      :further (beat-further old-beat changes))))

(defn- set-inst [inst play]
  (swap! channels (fn [v] (assoc v inst play))))


(defn- msg-bool [{[arg] :args :as msg}]
  (not (zero? arg)))

(defn- msg-scale [arg s e]
  (+ s (* arg (- e s))))

(defmacro defhandler [path binding-forms & body]
  (let [[binding-form val-var val-form & rest-forms] binding-forms]
   `(let [path# (str ~path)]
      (osc-handle server path#
                  (fn [msg#]
                    (let [~binding-form msg#
                          ~val-var ~val-form
                          ~@rest-forms]
                      (swap! handlers (fn [ov#] (assoc-in ov# [path# :value] ~val-var)))
                      ~@body))))))

(defn get-osc-value [path] (get-in @handlers [path :value]))

(definst trem [freq 440 depth 10 rate 6 length 3]
  (* 0.3
     (saw (+ freq (* depth (sin-osc:kr rate))))))

(definst kick [freq 120 dur 0.3 width 0.5]
  (let [freq-env (* freq (env-gen (perc 0 (* 0.99 dur))))
        env (env-gen (perc 0.01 dur) 1 1 0 1 FREE)
        sqr (* (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
        src (sin-osc freq-env)
        drum (+ sqr (* env src))]
    (compander drum drum 0.2 1 0.1 0.01 0.01)))


(definst c-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

(def metro-pad (atom [(now)]))
(def metro (metronome 128))
(def new-bpm (atom (metro-bpm metro)))

(defn update-metro [t]
  (swap! metro-pad
         (cond (> (- t (first @metro-pad)) (* 5 1000))
               (fn [_] [t])
               :else (fn [times]
                       (let [ntimes (take 5 (cons t times))
                             bpm (Math/round (/ (- (count ntimes) 1)
                                                (/ (- (first ntimes) (last ntimes)) (* 60.0 1000))))]
                         (metro-bpm metro bpm)
                         ntimes)))))

(defn osc-update-metro [msg]
  (let [arg (first (:args msg))]
    (if (= 1.0 arg)  (update-metro (now)))))

;;; {:src-port 52836, :src-host Tims-iPad.home, :path /3/rotary1, :type-tag f, :args (0.22182098)}
(defn osc-update-metro2 [{[arg] :args :as msg}]
  (let [min 30
        max 220
        bpm (Math/round (+ min (* arg (- max min))))]
    (osc-send client "/3/val1" bpm)
    (swap! new-bpm (fn [old-bpm] bpm))))

(def beat-pattern (atom [[overtone.inst.drum/dub-kick    [0 0 0 0 0 0 0 0]]
                         [soft-hat                       [0 0 0 0 0 0 0 0]]
                         [haziti-clap                    [0 0 0 0 0 0 0 0]]
                         [open-hat                       [0 0 0 0 0 0 0 0]]
                         [overtone.inst.drum/noise-snare [0 0 0 0 0 0 0 0]]
                         [overtone.inst.drum/bing        [0 0 0 0 0 0 0 0]]
                         [overtone.inst.drum/snare2      [0 0 0 0 0 0 0 0]]
                         ]))

(def beat-pattern-buffer (atom @beat-pattern))
;; (overtone.inst.drum/dance-kick)
;; (overtone.inst.drum/haziti-clap)
;; (overtone.inst.drum/dub-kick)
;; (overtone.inst.drum/noise-snare)
;; (overtone.inst.drum/quick-kick)
;; (overtone.inst.drum/snare)
;; (overtone.inst.drum/tone-snare)
;; (overtone.inst.drum/open-hat)
;; (overtone.inst.drum/kick2)
;; (overtone.inst.drum/kick3)
;; (overtone.inst.drum/bing)
;; (overtone.inst.drum/kick4)
;; (overtone.inst.drum/dry-kick)
;; (overtone.inst.drum/kick)
;; (overtone.inst.drum/hat3)
;; (overtone.inst.drum/soft-hat)
;; (overtone.inst.drum/clap)
;; (overtone.inst.drum/hat-demo)
;; (overtone.inst.drum/closed-hat)
;; (overtone.inst.drum/snare2)
;; (overtone.inst.drum/closed-hat2)
;; (overtone.inst.drum/tom)





(defn- update-osc [offset pattern]
  (dorun (map-indexed (fn [idx [inst pat]]
                        (dorun (map-indexed (fn [m v]
                                              (let [path (format "/4/multitoggle1/%d/%d"
                                                                 (+ idx 1) (+ m 1 offset))]
                                          (osc-send client path v)))
                                      pat)))
                      pattern)))

(defn update-osc-bp []
  (update-osc 0 @beat-pattern))

(defn update-osc-bpb []
  (update-osc 8 @beat-pattern-buffer))



(defn update-beat-pattern []
  (swap! beat-pattern
         #(vec (map (fn [[inst beat] [_ base-beat]]
                      [inst (new-beat base-beat beat)])
                    %1
                    @beat-pattern-buffer)))
  (update-osc-bp))

(defn- update-beat-pattern-from-buffer []
  (swap! beat-pattern (fn [bp] @beat-pattern-buffer))
  (update-osc-bp))

(defn pattern-to-timing [[inst pat]]
  (let [sub-pat (/ (count pat))]
    (keep
     (fn [[v  sub-b]]
       (if-not (zero? v)
         (vector sub-b inst)))
     (map #(vector %1 %2)
          pat
          (range 0 1 sub-pat)))))

(defn- update-metro-bpm []
  (let [old-bpm (metro-bpm metro)
        bpm-diff (- @new-bpm old-bpm)
        updated-bpm (+ old-bpm (Math/round (* 0.75 bpm-diff)))]
    (if (not (zero? bpm-diff))
      (metro-bpm metro updated-bpm))))

(defn play-pattern [beat]
  (update-metro-bpm)
  (update-beat-pattern)
  (let [next-beat (inc beat)
        pats (reduce into (map pattern-to-timing (filter identity (map (fn [play bp] (and play bp)) @channels @beat-pattern))))
        pats (sort #(compare (first %1) (first %2)) pats)]
    (doseq [[b inst] pats]
      (let [b (float b)]
        (at (metro (+ beat b)) (inst))))
    (apply-by (metro next-beat) #'play-pattern [next-beat])
    ))

(defn toggle-player [{path :path [arg] :args :as msg}]
  (case arg
    0.0 (stop)
    (play-pattern (metro))))

(defn handle-beat-pattern [{ path :path [arg] :args :as msg}]
  (let [[_ inst sub-beat] (re-find #"(\d+)/(\d+)$" path)
        v (if (zero? arg) 0 1)
        inst (- (read-string inst) 1)
        sub-beat (- (read-string sub-beat) 1 8)]
    (swap! beat-pattern-buffer
           (fn [bp]
             (assoc-in bp [inst 1 sub-beat] v)))))

(defn- randomize-buffer2 []
  (swap! beat-pattern-buffer
         (fn [old-bp]
           (let [ninst (count old-bp)
                 pat-len (count (second (first old-bp)))
                 inst-beat (repeatedly pat-len #(rand-int (+ 1 ninst)))]
             (vec (map-indexed (fn [idx [inst pat]]
                                 (vector inst (vec (map #(if (= %1 idx) 1 0) inst-beat))))
                               old-bp)))))
  (update-osc-bpb))

(comment
  (player (metro))
  (play-pattern (metro))
  (tplay (inc (metro)))


  (metro-bpm metro 60)
  (stop)
  (metro)

  (zero-conf-on)
  (zero-conf-off)


  (for [inst (range 8)
        sub-beat (range 9 17)]
    (osc-handle server (format "/4/multitoggle1/%d/%d" inst sub-beat) #'handle-beat-pattern))

  ;;; (osc-rm-all-handlers server "/4/multitoggle1")
  ;;;(osc-send client "/1/fader4" (/  32.0))

  ;;; (osc-listen server (fn [msg] (println msg)) :debug)
  (osc-rm-listener server :debug)
  ;; (osc-handle server "/7/push13" osc-update-metro)
  (osc-handle server "/3/toggle0" toggle-player)
  (osc-handle server "/3/rotary1" osc-update-metro2)
  (osc-handle server "/4/push2" (fn [{[arg] :args :as msg}]
                                  (if-not (zero? arg)
                                    (update-beat-pattern-from-buffer))))
  (osc-handle server "/4/nav4" (fn [{[arg] :args :as msg}]
                                  (if-not (zero? arg)
                                    (randomize-buffer2))))

  (doseq [i (range 8)]
    (defhandler (format "/4/multifader1/%d" (inc i)) [{[arg] :args :as msg}
                                               val arg]
      (update-closer-probs i val)))

  (defhandler "/4/fader5" [{[arg] :args :as msg}
                           val arg]
    (update-change-probs 0 val))

  (doseq [i (range 8)]
   (defhandler (format "/4/toggle%d" i) [{[arg] :args :as msg}
                                         val (msg-bool msg)]
     (set-inst (- i 1) val)))

  ;; (osc-handle server "/3/toggle1" osc-set-bpm)

  (osc-rm-handler server "/7/push13")
  (osc-close server)
  (osc-close client)
  (osc-send client "/4/multitoggle1/3/1" 1)

;;; keys on the side {:src-port 53459, :src-host Tims-iPad.home, :path /4/toggle1, :type-tag f, :args (0.0)}
;;; down arrow (lower right) {:src-port 53459, :src-host Tims-iPad.home, :path /4/nav4, :type-tag f, :args (0.0)}
;;; Fold {:src-port 61177, :src-host Tims-iPad.home, :path /4/push2, :type-tag f, :args (1.0)}
;;; {:src-port 52918, :src-host Tims-iPad, :path /7/push13, :type-tag f, :args (0.0)}
;;; {:src-port 60948, :src-host Tims-iPad.home, :path /4/multitoggle1/1/1, :type-tag f, :args (1.0)}
;;; {:src-port 60948, :src-host Tims-iPad.home, :path /4/multitoggle1/8/16, :type-tag f, :args (1.0)}
  )




;; (defn player [beat]
;;   (let [old-bpm (metro-bpm metro)
;;         bpm-diff (- @new-bpm old-bpm)
;;         updated-bpm (+ old-bpm (Math/round (* 0.75 bpm-diff)))]
;;     (if (not (zero? bpm-diff))
;;       (metro-bpm metro updated-bpm)))
;;   (at (metro beat) (kick))
;;   (at (metro (+ 0 beat)) (c-hat))
;;   (at (metro (+ 1/4 beat)) (c-hat))
;;   (at (metro (+ 1/2 beat)) (c-hat))
;;   (at (metro (+ 3/4 beat)) (c-hat))
;;   (let [nbeat (inc beat)]
;;     (apply-by (metro nbeat) #'player [nbeat])))

;; (defn player [beat]
;;   (let [old-bpm (metro-bpm metro)
;;         bpm-diff (- @new-bpm old-bpm)
;;         updated-bpm (+ old-bpm (Math/round (* 0.75 bpm-diff)))]
;;     (if (not (zero? bpm-diff))
;;       (metro-bpm metro updated-bpm)))
;;   (let [next-beat (inc beat)]
;;     (at (metro beat)
;;         (kick)
;;         (if (zero? (mod beat 2))
;;           (open-hat :amp 0.1)))
;;     (at (metro (+ 0.5 beat))
;;         (haziti-clap :decay 0.05 :amp 0.3))

;;     (when (zero? (mod beat 3))
;;       (at (metro (+ 0.75 beat))
;;           (soft-hat :decay 0.03 :amp 0.2)))

;;     (when (zero? (mod beat 8))
;;       (at (metro (+ 1.25 beat))
;;           (soft-hat :decay 0.03)))

;;     (apply-by (metro next-beat) #'player [next-beat])))

;; (defn- randomize-buffer []
;;   (swap! beat-pattern-buffer
;;          (fn [old-bp]
;;            (vec (map (fn [[inst pat]]
;;                        (vector inst (vec (repeatedly (count pat) #(rand-int 2)))))
;;                      old-bp))))
;;   (update-osc 8 @beat-pattern-buffer))
