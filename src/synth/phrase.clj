(ns synth.phrase
  (:use
   [overtone.live]
   [overtone.inst.sampled-piano]))



(def piano sampled-piano)








;; c0  d2  e4  f5  g7  a9  b11
;; 11  1   3   5   6*  8   10    0->11  Gb/F#
;; 0   1*  3   5   6   8   10    7->6   Db/C#
;; 0   1   3   5   7   8*  10    2->1   Ab/G#
;; 0   2   3*  5   7   8   10    9->8   Eb/D#
;; 0   2   3   5   7   9   10*   4->3   Bb/A#
;; 0   2   4   5*  7   9   10   11->10  F/E#

;; 0*  2   4   5   7   9   11           C/B#

;; 0   2   4   6   7*  9   11    5->6   G
;; 1   2*  4   6   7   9   11    0->1   D
;; 1   2   4   6   8   9*  11    7->8   A
;; 1   3   4*  6   8   9   11    2->3   E/Fb
;; 1   3   4   6   8   10  11*   9->10  B/Cb
