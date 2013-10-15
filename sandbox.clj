(ns user
  (:use overtone.live
        [overtone.inst.sampled-piano :only [sampled-piano]]
        overtone.inst.synth)

    )
    ;(:use overtone.core)

(def server (osc-server 44100 "osc-clj"))

(comment
  (def client (osc-client "192.168.1.5" 9801))
  (osc-listen server (fn [msg] (println msg)) :debug)
  (osc-close server)
  (osc-close client)
  )
(def multitoggle1 (atom 0))
(def multitoggle2 (atom 0))

(defn handle-multitoggle1-y1 [{:keys [src-port src-host path type-tag args]}]
  (let [f (case (int (first args))
            1 bit-set
            0 bit-clear)
        pos (- (read-string (re-find #"(?<=/)\d(?=/1)" path)) 1)]
    (swap! multitoggle1 f pos)
    (osc-send client "/1/fader4" (/ @multitoggle1 32.0))
    )
  )

(defn handle-multitoggle2-y1 [{:keys [src-port src-host path type-tag args]}]
  (let [f (case (int (first args))
            1 bit-set
            0 bit-clear)
        pos (read-string (re-find #"(?<=/)\d(?=/1)" path))]
   (swap! multitoggle2 f pos)))

(defn handle-push1-6 [{:keys [src-port src-host path type-tag args]}]
  (let [pushn (read-string (re-find #"(?<=push)\d+" path))]
    (if (= (int (first args)) 1)
      (case @multitoggle1
        0 (case pushn
            1 (stop)
            nil)
        1 (pink-noisey)
        2 (noisey)
        3 (toner (midi->hz (+ 68 pushn)))
        4 (sq (midi->hz (+ 68 pushn)))
        5 (triangular (midi->hz (+ 68 pushn)))
        6 (sawzall (midi->hz (+ 68 pushn)))))))

(defn handle-push7-12 [{:keys [src-port src-host path type-tag args]}]
  (let [pushn (read-string (re-find #"(?<=push)\d+" path))]
    (case @multitoggle2
     (case pushn
       1 (stop)
       nil))))


(def launcher (atom (vec (repeat 56 0))))

(defn handle-live-launcher-push [{:keys [src-port src-host path type-tag args]}]
  (let [pushn (- (read-string (re-find #"(?<=push)\d+" path)) 1)
        release (= (int (first args)) 0)
        v (case (nth @launcher pushn) 0 1 1 0)]
    (if release
      (do (swap! launcher assoc pushn v)
          (osc-send client path (nth @launcher pushn))))))

(doseq [i (range 1 7)]
  (osc-handle server (str "/1/push" i) handle-push1-6))
(doseq [i (range 7 13)]
  (osc-handle server (str "/1/push" i) handle-push7-12))
(doseq [x (range 1 6)]
  (osc-handle server (format "/1/multitoggle1/%d/1" x) handle-multitoggle1-y1))

(do (doseq [x (range 1 56)]
      (osc-rm-handler server (format "/1/push%d" x)))
    (doseq [x (range 1 56)]
      (osc-handle server (format "/1/push%d" x) handle-live-launcher-push)))




(doseq [i (range 1 7)]
  (osc-rm-handler server (str "/1/push" i)))
(doseq [i (range 7 13)]
  (osc-rm-handler server (str "/1/push" i)))
(doseq [x (range 1 6)]
  (osc-rm-handler server (format "/1/multitoggle1/%d/1" x)))
(doseq [x (range 1 6)]
  (osc-rm-handler server (format "/1/multitoggle2/%d/1" x)))

(comemnt
 (osc-handle server "/1/push1" (fn [_] (stop)))
 (zero-conf-off)
 (connect-external-server 57110)
 )

(definst c-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))


(definst o-hat [amp 0.8 t 0.5]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

; define a metronome at a given tempo, expressed in beats per minute.
(def metro (metronome 120))

(defn swinger [beat]
  (at (metro beat) (o-hat))
  (at (metro (inc beat)) (c-hat))
  (at (metro (+ 1.65 beat)) (c-hat))
  (apply-at (metro (+ 2 beat)) #'swinger (+ 2 beat) []))

;; (osc-handle server "/1/push2" (fn [_] (swinger (metro))))
;; (osc-handle server "/1/fader4" (fn [{:keys [src-port src-host path type-tag args]}]
;;                                  (metro-bpm metro (+ 60 (* 200 (first args))))))


(swinger (metro))
(stop)


(def boom (sample (freesound-path 33637)))
(def clap (sample (freesound-path 48310)))
(def clap2 (sample (freesound-path 132676)))
(def click (sample (freesound-path 406)))
(def close-hat (sample (freesound-path 802)))
(def cymbal (sample (freesound-path 13254)))
(def explosion (sample (freesound-path 80401)))
(def jetbike (sample (freesound-path 9088)))
(def kick (sample (freesound-path 777)))
(def kick2 (sample (freesound-path 2086)))
(def open-hat (sample (freesound-path 26657)))
(def open-snare (sample (freesound-path 16309)))
(def powerwords (sample (freesound-path 8323)))
(def ride (sample (freesound-path 436)))
(def sleigh-bells (sample (freesound-path 44293)))
(def snap (sample (freesound-path 87731)))
(def snare (sample (freesound-path 26903)))
(def steam-whistles (sample (freesound-path 30628)))
(def subby (sample (freesound-path 25649)))
(def two-cows (sample (freesound-path 16568)))
(def water-drops (sample (freesound-path 50623)))
(def witch-cackle (sample (freesound-path 80187)))

(piano 73)

(boom)
(clap)
(clap2)
(click)
(close-hat)
(cymbal)
(explosion)
(jetbike)
(kick)
(kick2)
(open-hat)
(open-snare)
(powerwords)
(ride)
(sleigh-bells)
(snap)
(snare)
(steam-whistles)
(subby)
(two-cows)
(water-drops)
(witch-cackle)

(definst overpad [note 60 amp 0.7 attack 0.001 release 2]
  (let [freq  (midicps note)
        env   (env-gen (perc attack release) :action FREE)
        env   (line:kr 1 0 3 FREE)
        f-env (+ freq (* 3 freq (env-gen (perc 0.012 (- release 0.1)))))
        bfreq (/ freq 2)
        sig (* 0.7 (+ (sin-osc freq) (sin-osc (* 0.99 freq))))
        sig   (apply +
                     (concat (* 0.7 (sin-osc [bfreq (* bfreq 0.99)]))
                             (lpf (saw [freq (* freq 1.01)]) f-env)))
        audio (* amp env sig)]
    audio))


(overpad 64 :attack 2 :release 4)

(defn player [beat notes]
  (let [notes (if (empty? notes)
                [50 55 53 50]
                notes)]
    (at (metro beat)
        (kick))
    (at (metro beat)
        (if (zero? (mod beat 5))
          (overpad (+ 24 (choose notes)) 0.2 0.75 0.005)))
    ;; (at (metro (+ 0.5 beat))
    ;;     (if (zero? (mod beat 6))
    ;;       (overpad (+ 12 (choose notes)) 0.5 0.15 0.1)
    ;;       (overpad (choose notes) 0.5 0.15 0.1)))
  (apply-at (metro (inc beat)) #'player (inc beat) (next notes) [])))



(player (metro) [70 71 72 73])
(stop)


;; model a plucked string. this is really cool!
(definst plucked-string [note 60 amp 0.8 dur 2 decay 30 coef 0.3 gate 1]
  (let [freq   (midicps note)
        noize  (* 0.8 (white-noise))
        dly    (/ 1.0 freq)
        plk    (pluck noize gate dly dly decay coef)
        dist   (distort plk)
        filt   (rlpf dist (* 12 freq) 0.6)
        clp    (clip2 filt 0.8)
        reverb (free-verb clp 0.4 0.8 0.2)]
    (* amp (env-gen (perc 0.0001 dur)) reverb)))

(plucked-string 64)

(defsynth foo [freq 200 dur 0.5]
  (let [src (saw freq)
        low (sin-osc (/ freq 2))
        filt (lpf src (line:kr (* 10 freq) (/ freq 3) 5))
        env (env-gen (perc 0.1 (/ dur 2)) :action FREE)]
    (out 0 (pan2 (* 0.8 src env)))))
(foo 440 10)

(definst bar [] (sin-osc [440 442]))

(definst toner [freq 440]
  (* (env-gen (perc 0.1 1.8) :action FREE)
     (sin-osc freq)))

(definst pink-noisey []
  (* (env-gen (perc 0.1 1.8) :action FREE)
     (pink-noise)))

(definst noisey []
  (* (env-gen (perc 0.1 1.8) :action FREE)
     (white-noise)))

(definst sq [freq 120]
  (* (env-gen (perc 0.1 4.8) :action FREE)
     (square freq)))

(definst triangular [freq 120]
  (* (env-gen (perc 0.1 4.8) :action FREE)
     (lf-tri freq)))


(definst sawzall [freq 440]
  (* (env-gen (perc 0.1 0.8) :action FREE)
     (saw freq)))



(defmacro bin:kr [bus & body]
  `(in:kr (bus-id ~bus) ~@body))

;; A fully server-side sample sequencer.
;; =====================================

;; This example demonstrates some of the benefits of moving all synth
;; triggers inside the server itself. For example, it allows you to
;; modify the synthesis with *immediate* effect (rather than waiting for
;; the next bar/chunk to be scheduled) and you can use a global pulse to
;; drive both the timing and to also modulate aspects of the synthesis
;; so that the modulations are sympathetic to the rhythms being played.

;; First, let's create some sequencer buffers for specifying which beat
;; to trigger a sample. This will be our core data structure for a basic
;; emulation of an 8-step sequencer. A buffer is like a Clojure vector
;; except it lives on the server and may only contain floats. Buffers
;; are initialised to have all values be 0.0
(defonce buf-0 (buffer 8))
(defonce buf-1 (buffer 8))
(defonce buf-2 (buffer 8))
(defonce buf-3 (buffer 8))

;; Next let's create some timing busses. These can be visualised as
;; 'patch cables' - wires that carry pulse signals that may be
;; arbitrarily forked and fed into any synth that wants to be aware of
;; the pulses. We have two types of information being conveyed here -
;; firstly the trg busses contain a stream of 0s with an intermittant 1
;; every time there is a tick. Secondly we have the cnt busses which
;; contain a stream of the current tick count. We then have two of each
;; type of bus - one for a high resolution global metronome, and another
;; for a division of the global metronome for our beats.
(defonce root-trg-bus (control-bus)) ;; global metronome pulse
(defonce root-cnt-bus (control-bus)) ;; global metronome count
(defonce beat-trg-bus (control-bus)) ;; beat pulse (fraction of root)
(defonce beat-cnt-bus (control-bus)) ;; beat count

;; Here we design synths that will drive our pulse busses.
(defsynth root-trg [rate 100]
  (out:kr root-trg-bus (impulse:kr rate)))

(defsynth root-cnt []
  (out:kr root-cnt-bus (pulse-count:kr (bin:kr root-trg-bus))))

(defsynth beat-trg [div BEAT-FRACTION]
  (out:kr beat-trg-bus (pulse-divider (bin:kr root-trg-bus) div))  )

(defsynth beat-cnt []
  (out:kr beat-cnt-bus (pulse-count (bin:kr beat-trg-bus))))

(def BEAT-FRACTION "Number of global pulses per beat" 30)

;; Now we get a little close to the sounds. Here's four nice sounding
;; samples from Freesound.org
;; (def kick-s (sample (freesound-path 777)))
;; (def click-s (sample (freesound-path 406)))
;; (def boom-s (sample (freesound-path 33637)))
;; (def subby-s (sample (freesound-path 25649)))

(def bsize (buffer-size buf-0))
;; Here's a synth for playing back the samples with a bit of modulation
;; to keep things interesting.
(defsynth mono-sequencer
  "Plays a single channel audio buffer."
  [buf 0 rate 1 out-bus 0 beat-num 0 sequencer 0 amp 1]
  (let [cnt      (bin:kr beat-cnt-bus)
        idx      (mod cnt 8)
        beat-trg (bin:kr beat-trg-bus)
        bar-trg  (and (buf-rd:kr 1 sequencer idx)
                      (= beat-num idx)
                      beat-trg)
        vol      (set-reset-ff bar-trg)]
    (out
     out-bus (* vol
                amp
                (pan2
                 (rlpf
                  (scaled-play-buf 1 buf rate bar-trg)
                  (demand bar-trg 0 (dbrown 200 20000 50 INF))
                  (lin-lin:kr (lf-tri:kr 0.01) -1 1 0.1 0.9)))))))

;; Here's Dan Stowell's dubstep synth modified to work with the global
;; pulses
(definst dubstep [note 40 wobble BEAT-FRACTION hi-man 0 lo-man 0 sweep-man 0 deci-man 0 tan-man 0 shape 0 sweep-max-freq 3000 hi-man-max 1000 lo-man-max 500 beat-vol 0 lag-delay 0.5]
  (let [bpm     300
        wob     (pulse-divider (bin:kr root-trg-bus) wobble)
        sweep   (lin-lin:kr (lag-ud wob 0.01 lag-delay) 0 1 400 sweep-max-freq)
        snd     (mix (saw (* (midicps note) [0.99 1.01])))
        snd     (lpf snd sweep)
        snd     (normalizer snd)

        snd     (bpf snd 1500 2)
        ;;special flavours
        ;;hi manster
        snd     (select (> hi-man 0.05) [snd (* 4 (hpf snd hi-man-max))])

        ;;sweep manster
        snd     (select (> sweep-man 0.05) [snd (* 4 (hpf snd sweep))])

        ;;lo manster
        snd     (select (> lo-man 0.05) [snd (lpf snd lo-man-max)])

        ;;decimate
        snd     (select (> deci-man 0.05) [snd (round snd 0.1)])

        ;;crunch
        snd     (select (> tan-man 0.05) [snd (tanh (* snd 5))])

        snd     (* 0.5 (+ (* 0.8 snd) (* 0.3 (g-verb snd 100 0.7 0.7))))
        ]
    (normalizer snd)))

;; Here's a nice supersaw synth
(definst supersaw2 [freq 440 amp 1 fil-mul 2 rq 0.3]
  (let [input  (lf-saw freq)
        shift1 (lf-saw 4)
        shift2 (lf-saw 7)
        shift3 (lf-saw 5)
        shift4 (lf-saw 2)
        comp1  (> input shift1)
        comp2  (> input shift2)
        comp3  (> input shift3)
        comp4  (> input shift4)
        output (+ (- input comp1)
                  (- input comp2)
                  (- input comp3)
                  (- input comp4))
        output (- output input)
        output (leak-dc:ar (* output 0.25))
        output (normalizer (rlpf output (* freq fil-mul) rq))]

    (* amp output (line 1 0 10 FREE))))


;; OK, let's make some noise!

;; Now, let's start up all the synths:
(do
  (def r-cnt (root-cnt))
  (def b-cnt (beat-cnt))
  (def b-trg (beat-trg))
  (def r-trg (root-trg))

  (def kicks (doall
              (for [x (range 8)]
                (mono-sequencer :buf kick :beat-num x :sequencer buf-0))))

  (def clicks (doall
               (for [x (range 8)]
                 (mono-sequencer :buf click :beat-num x :sequencer buf-1))))

  (def booms (doall
              (for [x (range 8)]
                (mono-sequencer :buf boom :beat-num x :sequencer buf-2))))

  (def subbies (doall
                (for [x (range 8)]
                  (mono-sequencer :buf subby :beat-num x :sequencer buf-3)))))

(swinger (metro))
(stop)
;; An empty palatte to play with:
(defn silent-buf []
  (buffer-write! buf-0 [0 0 0 0 0 0 0 0])  ;; kick
  (buffer-write! buf-1 [0 0 0 0 0 0 0 0])  ;; click
  (buffer-write! buf-2 [0 0 0 0 0 0 0 0])  ;; boom
  (buffer-write! buf-3 [0 0 0 0 0 0 0 0])) ;; subby

(do
  (buffer-write! buf-0 [1 0 0 0 0 0 0 0])  ;; kick
  (buffer-write! buf-1 [0 0 0 0 0 0 0 0])  ;; click
  (buffer-write! buf-2 [1 0 0 0 0 0 0 0])  ;; boom
  (buffer-write! buf-3 [0 0 0 0 0 0 0 0])) ;; subby

;; try mixing up the sequences. Evaluate this a few times:
(defn random-buf []
  (buffer-write! buf-0 (repeatedly 8 #(choose [0 1])))
  (buffer-write! buf-1 (repeatedly 8 #(choose [0 1])))
  (buffer-write! buf-2 (repeatedly 8 #(choose [0 1])))
  (buffer-write! buf-3 (repeatedly 8 #(choose [0 1]))))
(random-buf)
(osc-handle server "/1/push2"
            (fn [{[v] :args}]
              (if (= v 1.0)
                (random-buf))))

;; and then to something interesting
(defn std-buf []
  (buffer-write! buf-0 [1 1 1 1 1 1 1 1])
  (buffer-write! buf-1 [1 0 1 0 0 1 1 0])
  (buffer-write! buf-2 [1 0 0 0 0 1 0 0])
  (buffer-write! buf-3 [1 0 0 0 0 0 1 0]))



(def metro (metronome 160))
(defsynth r1)

(std-buf)
(osc-handle server "/1/push3"
            (fn [{[v] :args}]
              (if (= v 1.0)
                (std-buf))))


;; try changing the rate of the global pulse (everything else will
;; follow suit):
(ctl r-trg :rate 75)
(ctl r-trg :rate 100)
(ctl r-trg :rate 300)
(ctl r-trg :rate 150)
(defn dovol [v]
  (doseq [k kicks] (ctl k :amp v))
  (doseq [s subbies] (ctl s :amp v))
  (doseq [b booms] (ctl b :amp v))
  (doseq [c clicks] (ctl c :amp v)))
(dovol 0.1)
(random-buf)
(std-buf)
(silent-buf)
(stop)
(osc-handle server "/1/rotary3"
            (fn [{:keys [src-port src-host path type-tag args]}]
              (let [rate (+ 70 (* (first args) 300))]
                (ctl r-trg :rate rate))))

;; get the dubstep bass involved:
(def tonic (atom 30))
(dubstep :note @tonic
         :wobble (* BEAT-FRACTION 1)
         :lo-man 1)

(kill dubstep)
(kill kicks)

(osc-handle server "/1/rotary12"
            (fn [{[v] :args}]
              (swap! tonic (fn [_] (int (+ 25 (* 25 v)))))
              (ctl dubstep :note @tonic)))
(osc-handle server "/1/rotary11"
            (fn [{[v] :args}]
              (ctl dubstep :wobble (* BEAT-FRACTION v))))

(osc-handle server "/1/toggle13"
            (fn [{[v] :args}]
              (ctl dubstep :lag-delay v)))
(osc-handle server "/1/toggle14"
            (fn [{[v] :args}]
              (ctl dubstep :deci-man v)))
(osc-handle server "/1/toggle16"
            (fn [{[v] :args}]
              (ctl dubstep :lo-man v)))
(osc-handle server "/1/toggle15"
            (fn [{[v] :args}]
              (ctl dubstep :hi-man v)))
;; go crazy - especially with the deci-man
(ctl dubstep
     :note 29
     :wobble (* BEAT-FRACTION )
     :lag-delay 0
     :hi-man 0
     :lo-man 1
     :deci-man 0)

;; Bring in the supersaws!

(def ssaw-rq 0.9)
(def ssaw-fil-mul 2)

;; Fire at will...
(supersaw2 (midi->hz 28) :amp 3 :fil-mul ssaw-fil-mul :rq ssaw-rq)
(supersaw2 (midi->hz 40) :amp 3 :fil-mul ssaw-fil-mul :rq ssaw-rq)
(supersaw2 (midi->hz 45) :amp 2 :fil-mul ssaw-fil-mul :rq ssaw-rq)
(supersaw2 (midi->hz 48) :amp 2 :fil-mul ssaw-fil-mul :rq ssaw-rq)
(supersaw2 (midi->hz 52) :amp 2 :fil-mul ssaw-fil-mul :rq ssaw-rq)
(supersaw2 (midi->hz 55) :amp 2 :fil-mul ssaw-fil-mul :rq ssaw-rq)
(supersaw2 (midi->hz 57) :amp 2 :fil-mul ssaw-fil-mul :rq ssaw-rq)
(supersaw2 (midi->hz 64) :amp 1 :fil-mul ssaw-fil-mul :rq ssaw-rq)
(supersaw2 (midi->hz 67) :amp 1 :fil-mul ssaw-fil-mul :rq ssaw-rq)
(supersaw2 (midi->hz 69) :amp 1 :fil-mul ssaw-fil-mul :rq ssaw-rq)

(osc-handle server "/1/push7"
            (fn [{[v] :args}]
              (if (= v 1.0)
                (supersaw2 (midi->hz (- @tonic 1))))))
(osc-handle server "/1/push8"
            (fn [{[v] :args}]
              (if (= v 1.0)
                (supersaw2 (midi->hz (+ @tonic 11))))))
(osc-handle server "/1/push9"
            (fn [{[v] :args}]
              (if (= v 1.0)
                (supersaw2 (midi->hz (+ @tonic 15))))))
(osc-handle server "/1/push10"
            (fn [{[v] :args}]
              (if (= v 1.0)
                (supersaw2 (midi->hz (+ @tonic 18))))))
(osc-handle server "/1/push11"
            (fn [{[v] :args}]
              (if (= v 1.0)
                (supersaw2 (midi->hz (+ @tonic 20))))))
(osc-handle server "/1/push12"
            (fn [{[v] :args}]
              (if (= v 1.0)
                (supersaw2 (midi->hz (+ @tonic 22))))))

(osc-handle server "/1/xy2"
            (fn [{[fil rq] :args}]
              (ctl supersaw2
                   :fil-mul (+ 1 (* fil 4))
                   :rq rq)))
;; modify saw params on the fly too...
;;(ctl supersaw2 :fil-mul 4 :rq 0.2)



(demo 5
     (let [src1      (sin-osc 440)
           src2      (sin-osc 880)
           root-trig (impulse:kr 100)
           t1        (pulse-divider:kr root-trig 80)
           t2        (pulse-divider:kr root-trig 20)]
       (pan2 (* 0.2
               (+ (* (decay t1 0.1) src1)
                  (* (decay t2 0.1) src2))))))

(def root-bus (control-bus))
(def beat-bus (control-bus))
(def root-cnt-bus (control-bus))
(def beat-cnt-bus (control-bus))

(defsynth root-trig [rate 100]
 (out:kr root-bus (impulse:kr rate)))

(defsynth beat-trig [d 30]
  (out:kr beat-bus (pulse-divider:kr (bin:kr root-bus) d)))

(defsynth root-cnt []
  (out:kr root-cnt-bus (pulse-count:kr (bin:kr root-bus))))

(defsynth beat-cnt []
  (out:kr beat-cnt-bus (pulse-count:kr (bin:kr beat-bus))))

(definst pingr [freq 440 div 20]
 (let [src1 (sin-osc freq)
       t1 (pulse-divider:kr (bin:kr beat-bus) div)]
   (* (decay t1 0.1) src1)))

(def r-trig (root-trig 100))
(def b-trig (beat-trig 30))
(def r-cnt (root-cnt))
(def b-cnt (beat-cnt))

(pingr)
(pingr 440 3)
(pingr 990 1)
(kill pingr)
(ctl r-trig :rate 40)
(stop)

(def c-bus (control-bus))

(defsynth root-trig [rate 100]
 (out:kr c-bus (impulse:kr rate)))

(definst pingr [freq 440 div 20]
 (let [src1 (sin-osc freq)
       t1 (pulse-divider:kr (in:kr c-bus) div)]
   (* (decay t1 0.1) src1)))

(def r-trig (root-trig))
(pingr)
(pingr 440 50)
(pingr 990 40)
(kill pingr)
(ctl r-trig :rate 50)
(stop)
;; play the instrument:
(thx)
;; kill it off when you're ready
(ctl thx :gate 0)



(def tremolo-bus (control-bus))
(def vibrato-bus (control-bus))
(defsynth tremolo-synth [freq 1 depth 1 ]
  (let [frac (/ depth 2.0)]
    (out:kr tremolo-bus (+ (- 1 frac) (* frac (sin-osc:kr freq))))))

(defsynth vibrato-synth [slew-rate 1 dfreq 1]
  (out:kr vibrato-bus (* dfreq (sin-osc:kr slew-rate))))


(do
  (def trem (tremolo-synth 2 1))
  (def vib (vibrato-synth 10 10))
  (def sig (lf-tri 240))
  (demo 5 (pan2 (* 0.05 (lf-tri (+ (bin:kr vibrato-bus) 240))))))

(definst harpsichord [freq 440]
  (let [duration 1
        snd  (string freq duration)
        t1   (* 0.2 (string (* 2/1 freq) duration))
        t2   (* 0.15 (string (* 3/2 freq) duration))
        t3   (* 0.1 (string (* 4/3 freq) duration))
        t4   (* 0.1 (string (* 5/4 freq) duration))
        snd  (+ snd (mix [t1 t2 t3 t4]))]
    snd))
(definst harpsichord2 [freq 440]
  (let [duration 1
        snd  (string freq duration)
        t1   (* 0.2 (string (* 2/1 freq) duration))
        t2   (* 0.15 (string (* 3/2 freq) duration))
        t3   (* 0.1 (string (* 4/3 freq) duration))
        t4   (* 0.1 (string (* 5/4 freq) duration))
        snd  (+ snd t1 t2 t3 t4)]
    snd))

(harpsichord (midi->hz 67))
(harpsichord2 (midi->hz 67))



(stop)


(defsynth feedback-loop []
  (let [input (crackle 1.5)
        fb-in (local-in 1)
        snd (+ input (leak-dc (delay-n fb-in 2.0 (* 0.8 (mouse-x 0.001 1.05)))))
        fb-out (local-out snd)
        snd (limiter snd 0.8)]
    (out 0 (pan2 snd))))

(comment

(bizzle b)
(compressor-demo b)
(stop)
(demo 3 (crackle 0.9))
(feedback-loop)
(stop)

)



(def ^:dynamic m (metronome 160))

(defn from [timing offset]
  #(timing (+ offset %)))

(defn speed-up [timing factor]
  (metro-bpm m (* factor (metro-bpm m)))
  timing)

(defn first-bit# [timing]
  (-> timing
    (from -1)
    (speed-up 2)
    (even-melody# (map ionian [2 4 5 4 4 2 4]))
    (from 9)
    (even-melody# (map ionian [-2 1 2 1 1 -2 1]))
    (from 9)
    (even-melody# (map ionian [-2 1 2 1 1 -2 1 2 3 4]))
    (from 6)
    (even-melody# (map ionian [-1 -2 -3 0 0 -3 0 1 0 -3])))
  (rhythm-n-bass# timing (take 8 (cycle progression))))

(defn play# []
  (binding [m (metronome 160)]
    (-> m (from 2) intro# first-bit#
        (speed-up 3/2) variation# final-chord#)))
(play#)
(binding [m (metronome 160)] (intro# (from m 2)))


(defn ppdoc [s]
  (pprint (filter #(re-find (re-pattern s) (str %)) (keys (ns-publics (find-ns 'overtone.live))))))

(definst foo-inst [pitch 440]
  (* 0.1 (sin-osc pitch)))

(defsynth foo-synth [pitch 440 amp 0.5]
  (out 0 (pan2 (* amp
                  (env-gen (perc 0.1 0.5 0.3) :action FREE)
                  (sin-osc pitch)))))

(defsynth new-foo-synth [pitch 440 amp 0.5]
  (out 0 (pan2 (* amp
                  (env-gen (perc 0.1 0.5 0.3) :action FREE)
                  (+ (* 0.3 (lf-saw (* pitch 0.99)))
                     (sin-osc pitch))
                  ))))

(foo-synth 440 1)
(new-foo-synth 220 0.3)
(stop)
(defsynth new-foo-synth [pitch 440]
  (let [src (in:ar 0)]
    (out:ar 0 (comb-n src 1 0.5 2))))

(defsynth word-player [start 0 duration 1 pulse 2 rate 1]
  (out 0 (* (play-buf 2 (buffer-id powerwords) rate 1 (* 44100 start) 0)
            (env-gen (envelope [1 1 0] [duration 1] :step) :action FREE))))


(defcgen love [pitch {:default 1}]
  (:ar (* (pitch-shift (play-buf 2 (buffer-id powerwords) 1 1 (* 44100 3.8) 0) :pitch-ratio pitch)
          (env-gen (envelope [1 1 0] [1.5 0.1] :step) :action FREE)))
  (:default :ar))

(defsynth newfoo [pitch 64]
  (let [source (in 58)
        echo (comb-n source 1 0.5 2)]
    (replace-out 58 (pan2 (+ echo source) 0))))

(sampled-piano)
(:id (:bus sampled-piano))
(fx-echo :tgt (:fx-group sampled-piano) :pos :tail :bus 50)
(:fx-chain sampled-piano)
(inst-fx! sampled-piano fx-echo)
(clear-fx sampled-piano)
(plucked-string 50)
(newfoo)
(stop)
(at (+ (now) (* 1 1500)) )

(let [t (+ (now) 1000)]
  (doseq [pr (range 0 10)]
    (demo 1 (love (+ 0.7 (* pr 0.1))))
    (Thread/sleep 1200)))

(p (map
    #(assoc % :synth ks1-demo)
    (pattern [[D5 D5 D5] [B4 [A4 B4] G4]] 2)))

(def piano-state (atom {:octave 5}))

;; {:src-port 63747, :src-host 192.168.1.152, :path /8/push3, :type-tag f, :args (0.0)}

(defn handle-piano-key [{:keys [src-port src-host path type-tag args]}]
  (let [n (+ (read-string (re-find #"(?<=/push)\d+" path)) (* (:octave @piano-state) 12) -1)]
    (if (> (first args) 0)
      (let [p (sampled-piano n)]
        (swap! piano-state assoc n p))
      (let [p (@piano-state n)]
        (ctl p :gate 0)
        (swap! piano-state dissoc n)))
    )
  )

(defn handle-octave-change [{:keys [src-port src-host path type-tag args]}]
  (let [n (read-string (re-find #"(?<=/nav)\d+" path))
        current-octave (@piano-state :octave)]
    (if (> (first args) 0)
      (swap! piano-state assoc :octave (if (= n 1)
                                        (+ current-octave 1)
                                        (- current-octave 1))))))

(osc-rm-all-handlers server)
(doseq [i (range 1 13)] (osc-handle server (str "/8/push" i) handle-piano-key))
(doseq [i (range 1 3)] (osc-handle server (str "/8/nav" i) handle-octave-change))


(use 'overtone.inst.synth)

(definst b3
  [note 60 amp 0.3 dur 0.4]
  (let [freq  (midicps note)
        waves (sin-osc [(* 0.5 freq)
                        freq
                        (* (/ 3 2) freq)
                        (* 2 freq)
                        (* freq 2 (/ 3 2))
                        (* freq 2 2)
                        (* freq 2 2 (/ 5 4))
                        (* freq 2 2 (/ 3 2))
                        (* freq 2 2 2)])
        snd   (apply + waves)
        env   (env-gen (envelope [0 amp amp 0] [0.01 dur 0.1]) :action FREE)]
    (* env snd 0.1)))


(use 'synth.mad :reload)
(use 'synth.violin :reload)
(def pinst b3)
(play-with-inst [dox-suprano pinst 15
                 dox-bass pinst 15
                 dox-alto pinst 15
                 dox-tenor pinst 15
                 ])

(ctl r-trig :rate 100)

(demo 10
     (let [src1      (sin-osc 440)
           src2      (sin-osc 880)
           root-trig (impulse:kr 100)
           t1        (pulse-divider:kr root-trig 37)
           t2        (pulse-divider:kr root-trig 51)]
       (* 0.2
          (+ (* (decay t1 0.1) src1)

             (* (decay t2 0.1) src2)))))

(defsynth metro-synth [c-bus 0 rate 1]
  (let [trigger (impulse:kr rate)
        count (pulse-count trigger)
        stp (stepper:kr trigger)]
    (send-trig:kr trigger count)
    (out:kr c-bus trigger)))

(on-event "/tr" #(println "trigger: " %) ::metro-synth)

(metro-synth)


;; 2, 0, Note_on_c, 0, 64, 70
;; 2, 256, Note_on_c, 0, 64, 76
;; 2, 512, Note_on_c, 0, 65, 79
;; 2, 768, Note_on_c, 0, 67, 82
;; 2, 1024, Note_on_c, 0, 67, 79
;; 2, 1280, Note_on_c, 0, 65, 70
;; 2, 1536, Note_on_c, 0, 64, 69
;; 2, 1792, Note_on_c, 0, 62, 69
;; 2, 2048, Note_on_c, 0, 60, 73
;; 2, 2304, Note_on_c, 0, 60, 74
;; 2, 2560, Note_on_c, 0, 62, 78
;; 2, 2816, Note_on_c, 0, 64, 83
;; 2, 3072, Note_on_c, 0, 64, 78
;; 2, 3456, Note_on_c, 0, 62, 69
;; 2, 3584, Note_on_c, 0, 62, 74
;; 2, 4096, Note_on_c, 0, 64, 82
;; 2, 4352, Note_on_c, 0, 64, 76
;; 2, 4608, Note_on_c, 0, 65, 80
;; 2, 4864, Note_on_c, 0, 67, 79
;; 2, 5120, Note_on_c, 0, 67, 78
;; 2, 5376, Note_on_c, 0, 65, 70
;; 2, 5632, Note_on_c, 0, 64, 72
;; 2, 5888, Note_on_c, 0, 62, 70
;; 2, 6144, Note_on_c, 0, 60, 72
;; 2, 6400, Note_on_c, 0, 60, 73
;; 2, 6656, Note_on_c, 0, 62, 78
;; 2, 6912, Note_on_c, 0, 64, 83
;; 2, 7168, Note_on_c, 0, 62, 74
;; 2, 7552, Note_on_c, 0, 60, 71
;; 2, 7680, Note_on_c, 0, 60, 77
;; 2, 8192, Note_on_c, 0, 62, 85
;; 2, 8448, Note_on_c, 0, 62, 75
;; 2, 8704, Note_on_c, 0, 64, 80
;; 2, 8960, Note_on_c, 0, 60, 67
;; 2, 9216, Note_on_c, 0, 62, 81
;; 2, 9472, Note_on_c, 0, 64, 81
;; 2, 9600, Note_on_c, 0, 65, 81
;; 2, 9728, Note_on_c, 0, 64, 75
;; 2, 9984, Note_on_c, 0, 60, 67
;; 2, 10240, Note_on_c, 0, 62, 81
;; 2, 10496, Note_on_c, 0, 64, 79
;; 2, 10624, Note_on_c, 0, 65, 79
;; 2, 10752, Note_on_c, 0, 64, 75
;; 2, 11008, Note_on_c, 0, 62, 70
;; 2, 11264, Note_on_c, 0, 60, 71
;; 2, 11520, Note_on_c, 0, 62, 79
;; 2, 11776, Note_on_c, 0, 55, 51
;; 2, 12032, Note_on_c, 0, 64, 84
;; 2, 12288, Note_on_c, 0, 60, 55
;; 2, 12544, Note_on_c, 0, 64, 97
;; 2, 12800, Note_on_c, 0, 65, 100
;; 2, 13056, Note_on_c, 0, 67, 103
;; 2, 13312, Note_on_c, 0, 67, 101
;; 2, 13568, Note_on_c, 0, 65, 92
;; 2, 13824, Note_on_c, 0, 64, 89
;; 2, 14080, Note_on_c, 0, 62, 88
;; 2, 14336, Note_on_c, 0, 60, 93
;; 2, 14592, Note_on_c, 0, 60, 97
;; 2, 14848, Note_on_c, 0, 62, 100
;; 2, 15104, Note_on_c, 0, 64, 103
;; 2, 15360, Note_on_c, 0, 62, 94
;; 2, 15744, Note_on_c, 0, 60, 90
;; 2, 15872, Note_on_c, 0, 60, 93



(stop)