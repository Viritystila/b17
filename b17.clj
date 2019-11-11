(ns b17
  (:use [trigger.trigger]
        [trigger.synths]
        [trigger.algo]
        [trigger.speech]
        [trigger.samples]
        [trigger.trg_fx] [overtone.core])
  (:require [viritystone.tone :as t]))

(future
  (println "Begin loading SuperDirt samples")
  (load-all-SuperDirt-samples)
  (println "Samples loaded"))

(defn add-tts-sample [name path nosamples]
  (future
    (println "Begin loading sample " name)

    (add-sample name (string-to-buffer (generate-markov-text path nosamples)))
    (println "Sample" name "loaded") ))

0.5625

(set-pattern-duration (/ 1 (* 1 0.5625)))

(set-pattern-delay 0.3)

                                        ;;;;;;;;;;;;;
                                        ;;Sormikäsi;;
                                        ;;;;Ääni;;;;
                                        ;;;;;;;;;;;;;

(trg :bassd smp :in-amp [0])

(pause! :bassd)

( trg :bassd smp
      :in-trg ;[1 r [1 1 1 r] 1]
    ;[1 r r  [1 1 r r]]
     ;[[[1 1] r] r  [1 r] [1 1 r r]]
     ;[[1 1 r r] r [1 [1 1 1 1]] r]
     ;[1 r r [1 1 r r]]
     ;[[r r 1 1 r r r r] r]
    ;[[(rep 4 1)] r 1 [1 r] ]

 ;[1 1  r [1 1 1 1] 1]
 ;[1 1  r (dcl  [(rep 32 1) 1])]
 ;[(acc [(rep 16 1) r (dcl [(rep 32 1) r])])]

 (evr 16 [[(rep 8 1)] 1 1 [1 1 1 1]] (evr 8 [(rep 16 1)] (rep 16 [(rep 8 1)])))

                                         ;[1 1 r [(rep 8 1)] 1]
 ;[1 1  r (dcl  [(rep 16 1) 1])]
     :in-step [2]; (evr 8 (fll 4 [2 -1.75 -1.75 -2]) (rep 16 [2]))
     :in-loop [0]
     :in-start-pos [0]
 :in-buf (fll 16 (sfl ["b bass23" r r "b sn1" r "b bass23" r   "b sn0"]))
  (fll 16 (sfl ["b bass23" r r "b sn1" r "b bass23" r   "b sn0"]))
 (fll 16 (sfl ["b bass23" r r "b sn1" r "b bass23" r   "b sn0"]))
  (fll 8 (sfl ["b bass23" r r "b sn1" r "b bass23" r   "b sn0"]))
                                        ;(evr 3 ["b bass20"] (rep 16 [ "b bd1" ])) ; (fll 8 [ "b bd1"  "b bass23"]) ;[ "b bd1" ])
)


(play! :bassd)

(pause! :bassd)

(trg! :bassd :bassde trg-fx-echo :in-decay-time [(/ (/ 1 0.5626)  2)]  :in-delay-time  [(/ (/ 1 0.5626)  50)] :in-amp (evr 16 [0.125 0.5 0.75 1 0.75 0.5 1 0.125] (rep 16 [0.125])))

(stp :bassd)

(trg :snare smp)

(pause! :snare)

(trg :snare smp
     :in-trg [1 1 1 1]
     ;[r 1 r [1 [1 1]]]
     ;[r 1 r [r r 1 1]]
     ;[[r [1 1]]  [r 1]]
     ;[[1 1 r 1]  r [r 1 1 r] [(rep 8 1 )]]

     ;[[1 1 r r] r r [(rep 32 2)]]
     ;[1 r 1 [(rep 48 1)]]
     ;[[1 1 r r] r r [(rep 32 1)] r r [(rep 16 1)] r]
     ;[1 r 1 [(rep 16 2)]]
     :in-step [2]; [1 [1 1] 1.5 1.5]     ; [1.5] [1]       ;":in-trg2"
     :in-loop [0]
     :in-start-pos [0]
     :in-buf ["b bd0"]; (fll 32 ["b sn2" "bsn0"])
     :in-amp [1]       ; [0.15]
     )

(play! :snare)

(pause! :snare)

(stp :snare)

(trg! :snare :snaree trg-fx-echo :in-decay-time [(/ (/ 1 0.5626)  2)]  :in-delay-time  [(/ (/ 1 0.5626)  50)] :in-amp (evr 16 [0.125 0.5 0.75 1 0.75 0.5 1 0.125] (rep 16 [0.125])))

(sta)

(print (map find-note-name (chord-degree :i :c2 :major 8)))

(do
  (trg :tb303sn tb303)

  (pause! :tb303sn))

(trg :tb303sn
     tb303
     :in-trg
     ;["n d1" r ["n c2" "n d3"]  "n d3"]
     ;[["n d1" "nb1"] r "n c2"  "n d3"]
     ;["n d1" r ["n c2" "n d3"]  [r "n d3" r "n d1"]]
     ;[["n d1" "n d2"] r  ["n c2" "n e2" "n e1" "n e2"] ["n e2" "n d3"]]

     ; (slw 2  [ ["n d1" "n d2"] r  ["n c2" "n e2" "n e1" "n e2"]  ["n e2" "n d3"]])
     ;[(fll 32 ["n d3" "n d2"]) r r r  (fll 16 ["n e3" "n c2"]) r r r]

     ;[ ["nc4" "na3" ["n f3" "nd3"]] ["nb2" "ng2" ["ne2" "nc2"]]]

     (slw ["n c3" ["n c3" "n d3"]])
     (slw ["n e3" ["n c2"  "n c3"]])
     (slw 2 [["n e2" "n e3" r r] [r "n d3" "n e3" "n d2"]])
     (slw ["n d3" ["nd3" "nb2" "nf2" "ne3"]])

                                        ;[["n d1" "n d2"] ["n c2" "n e2" "n e1" "n e2"]]

     ;; (evr 4 ["n e2" "ne0" ["n e2" "n c2"]  ["n d2" "n e2"]]
     ;;      (rep 6 ["n d1" "ne0" ["n c1" "n d0"]  ["n d0" "n e1"]]))
     ;; ["n d1" "n c0" ["n c1" "n d0"]  ["n d0" "n e1" "n d2" "n c2"]]
     ;; [["n c1" "n d1"] r  ["n c0" "n e1" "n e1" "n f1"]  ["n b0" "n d1"]]
     :in-amp [1]
     :in-note  ":in-trg"
     :in-gate-select [1]
     :in-attack [0.001]
     :in-decay [0.019]
     :in-sustain [0.25]
     :in-release [5.73]
     :in-r [0.9]
     :in-cutoff [500]
     :in-wave  ;(rep 4 [0])
     (rep 4 [0]) ;(rep 2 [2])
     ;(rep 4 (fll 512 [0 1 2]))
     )

(play! :tb303sn)

(pause! :tb303sn)

(volume! :tb303sn 0.5)

(trg! :tb303sn :tb303e trg-fx-echo :in-decay-time [0.5]  :in-delay-time  [1] :in-amp [1])

(stp :tb303sn)

(stp :tb303e)

(trg :hhsmp smp :in-amp [0])

(pause! :hhsmp)

(lss)

(sta)

(do
  (add-sample "uh" (string-to-buffer "UHHHHHHHHAAAAAAAAAAAA"))

  (add-sample "ee" (string-to-buffer "EEEE"))

  (add-sample "aa" (string-to-buffer "AAAAAAAAAA"))

  (add-sample "oo" (string-to-buffer "OOOOOOOOOO"))

  (add-sample "uhea" (string-to-buffer "UHHHHHHHHEEEEEEEAAAAAAA")))

(trg :uhsmp smp)

(pause! :uhsmp)

(trg :uhsmp smp
     :in-trg

     [r 1 r [1 [1 1]]]
     [r 1 r [r r 1 1]]

                                        ;[r [r  1] 1 1]
     ;[r 1 r [r r 1 1]]
     ;[r [1 1] 1 [r 1]]
     ;[[(rep 4 1)]  r [1 1 r r] r]
     :in-loop [0]
     :in-buf ["b uhea"]
     ["b uh"] ["b oo"] ;(fst 16 ["b aa" "b ee"])
     :in-amp [0.5]
     :in-step [2]; (fst 32 [(range 1.5 2.5 0.25)])
     )

(play! :uhsmp)

(pause! :uhsmp)

(volume! :uhsmp 0.5)

(stp :uhsmp)

(trg :tick ping)

(pause! :tick)

(trg :tick ping :in-amp [0] :in-trg [(rep 60 1)])

(play! :tick)

(stp :tick)

(sta)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(trg :singlesmp smp
     :in-trg (acc 4 (fst 8 [2 r 3 4 5 r r 6])) (rep 7 [r])
      (dcl 2 (fst 8 [1 2 3 4 5 6 7 8])) (rep 7 [r])
      :in-loop (rep 8 [0]) [1] (rep 7 [0])
     :in-buf (fll 8 ["b hc0" "b sn1" "b bd0"])
     :in-step  (rep 8 [2]) [2] (rep 7 [2])
     :in-amp [0.25])

(stp :singlesmp)

(trg :uhsmp smp
     :in-trg

     [[(rep 4 1)] 1 [(rep 8 1)]]
     (rep 7 [r])

                                        ;[r [r  1] 1 1]
     ;[r 1 r [r r 1 1]]
     ;[r [1 1] 1 [r 1]]
     ;[[(rep 4 1)]  r [1 1 r r] r]
     :in-loop [0]
     :in-buf ["b uhea"]
     ["b uh"] ["b oo"] ;(fst 16 ["b aa" "b ee"])
     :in-amp [0.5]
     :in-step [2]; (fst 32 [(range 1.5 2.5 0.25)])
     )


(trg :ks1 ks1)

(pause! :ks1)

(trg :ks1
     ks1
     :in-trg
     (fst 16 ["nc2" "nc1" "nc1" "nb1"])
     (fst 16 ["nc2" "nc1" "nc0" "ne2"])
     (fst 16 ["nc2" "nd1" "nc0" "nd2"] )
     (fst 8 ["nc2" "nd1" "nc0" "nd2" "nc3" "nd2" "nc1" "nd3"])

     (fst 16 ["nc3" "nc1" "nc1" "nb1"])
     (fst 16 ["nc2" "nc2" "nc0" "ne2"])
     (fst 16 ["nc3" "nd1" "nc1" "nd2"] )
     (fst 8 ["nc4" "nd4" "nc3" "nd3" "nc2" "nd2" "nc1" "nd1"])
     :in-dur [10.5]
     :in-amp [1]
     :in-note ":in-trg"
     ;(rep 1 ["n f#3"])
     ;(rep 1 ["n d3"])
     ;(rep 1 ["n a3"])
     ;(rep 1 ["n c#4"])
     ;(rep 1 ["n f#2"])
     ;(rep 1 ["n d2"])
     ;(rep 1 ["n a2"])
     ;(rep 1 ["n c#3"])
     :in-decay [0.5]; [(range 0.01 1 0.01)]
     :in-coef [0.5] ;[(range 0.01 0.9 0.01)]
     )

(play! :ks1)

(volume! :ks1 1)

(trg! :ks1 :ks1f trg-fx-feedback :in-delay-time [0.125] :in-decay-time [10.5])

(stp :ks1f)

(stp :ks1)

(println (map find-note-name (chord-degree :ii :d4 :melodic-minor)))

(trg :op overpad)

(pause! :op)

(trg :op overpad
     :in-trg [1 1 1 1]
                                        ;[r]
     ;(map-in [(rep 8 [1 r r 1])] scl 0.5)
     :in-note  (rep 2 ["n f#2"])
     (rep 1 ["n d2"])
     (rep 4 ["n a2"])
     (rep 1 ["n c#3"])

     ;(rep 1 (fll 32 [ "n f#2"  "n f#1" ]))
     ;(rep 1 (fll 32 [ "n a2"  "n a1" ]))
     ;(rep 1 (fll 32 [ "n c#2"  "n c#3" ]))
     ;(rep 1 (fll 32 [ "n d1"  "n d2" ]))
     :in-gate-select [1]                 ;(rep 16 [1]) (rep 16 [0])
     :in-attack [0.001]
     :in-decay  [0.1]
     :in-sustain [0.154]
     :in-release [10.13]
     :in-amp [1])

(play! :op)

(volume! :op 0.45)

(trg! :op :ope trg-fx-echo :in-decay-time [0.25]  :in-delay-time [0.001] :in-amp [0.5])

(stp :ope)

(stp :op)


(add-tts-sample "k1"  "generalx2paradisedaqx2.txt" 200)


(add-tts-sample "k2"  "generalx2paradisedaqx2.txt" 200)


(add-tts-sample "k3"  "generalx2paradisedaqx2.txt" 200)


(trg :ksmp smp)

(pause! :ksmp)

(trg :ksmp smp
     :in-trg [(rep 128 1)][1 1 1 1 1 1 1 [(rep 64 1)]] (rep 6 [r])
     :in-buf ["b k2"]
     :in-step [2] ;(slw 1 [(sir 32 2.5 1 32)]) ;[2]
     :in-loop [1]
     :in-start-pos  [(range 0 404040 5000)]
     :in-amp [1.0])

(trg :gb
     grunge-bass
     :in-trg   ; [(rep 6 "a4")]
                                        ;(vec (repeat 2 (seq ["b4" "b4" "b4" ["e4" ["e4" "d4"]] ])))
                                        ;[["a4" "d4"] "e4" "b4" "b4"]
     (rep 2 [(rep 16 "n b2")])
     (rep 2 [(rep 16 "n d3")])
     (rep 2 [(rep 16 "n e2")])
     (rep 2 [(rep 16 "n a2")])
     ;(repeat  3 [r])
     ;["a5"]
     ;["b5"]
     ;["d5"]
     ;["e5" "d3" "b2" "b4"]
     :in-gate-select  [1] ; (rep 4 [1]) (rep 4 [0])
     :in-amp [5.5]
     :in-note  ":in-trg"
     :in-a [0.0001]
     :in-d [0.093]
     :in-s [0.195]
     :in-r [0.25]; (slw 32 [(range 0.1 1 0.01)])
     )

(pause! :gb)

(play! :ksmp)

(stp :ksmp)

(lss)

(sta)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(trg :kick kick)

(pause! :kick)

(trg :kick kick :in-trg   ; (rep 7 [(rep 16 1 )])
     ;[1 r 1 [1 1] 1 r 1 1 1 1 (acc [(rep 8 1)]) 1 1 1 1 [1 1 1 1]]
     ;[1 r r [(rep 16 1)]]
     ;[1 [(rep 8 1)] 1  r]
     ;[1 [1 1 1 1] [(rep 16 1)] r ]
     ;(rep 2 [(rep 8 1)])
     ;[1 1 1 r 1 1 1 [1 1 1 1]]

     (fst 4 [[1 2] r [3 4] r [5 6] r [7 8] 9])
     (fst 4 [[2 r 3 4] r [5 6 r 7] 1 [8 9 1 2 ] r [2 3] 4])
       (acc [(rep 64 1)])
       :in-f3  [ "fc1" "fg1" "f f1" "fbb1"]
        [ "fg1" "fc1" "f bb1" "ff1"]
        :in-f2 [100]
     :in-f1 (fll 32 [100 100])
     ;[ "fc2" "fg2" "ff2" "fbb2"]
     ;; [[ "fc3" "fg3" "ff3" "fbb3"]
     ;;  [ "fc3" "fg3" "ff3" "fbb3"]
     ;;  [ "fc4" "fg3" "ff2" "fbb1"]]
     :in-amp [0.25])

(volume! :kick 0.25)

(trg! :kick :kickd trg-fx-distortion
      :in-level [0.95]
      )

(play! :kick)

(pause! :kick)

(stp :kick)

(sta)

(trg :nh hat2
     :in-trg (rep 7 [(rep 16 1)])
     (fst 4 [[1 1] r [1 1] r [1 1] r [1 1] 1])
     (acc [(rep 64 1)])
     :in-attack [0.01 0.0001]
     :in-amp [1])

(stp :nh)


(trg :hz haziti-clap
     :in-trg  (rep 7 (fll 16 [0 2]))
     (fst 4 [[1 0] r [1 0] r [1 0] r [1 0] 1])
     :in-amp [0.5]
                                        ;[[1 1 1 1] 1 1 r]
     ;[(rep 8 1 )]
     ;[1 1 1 [1 1] 1 [1 1] 1 [1 1 1 1]]
     ;[1 1 [1 1 r 1] r 1]
     ;[[1 1]  1 1 [1 1 1 r] [1 1] r r 1]
     ;[1 [1 1] r [1 1 r [1 1] r 1 1]]
     ;(rep 5 [1 1 1 1])
     ;[(acc [(rep 16 1)]) 1 r [1 1]]
     )

(trg! :hz :hze trg-fx-feedback
      :in-delay [0.01]
      :in-decay [0.1])


(stp :hze)

(stp :hz)

(sta)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(trg :gb2
     vintage-bass
     :in-trg (map-in [(rep 1 1)] scl 0.1)
     ;(map-in [1 [1 1 1 1] r [1 r r [1 1 1 r]]] scl 0.1)
     ;(map-in [1 [1 r 1 [1 1]] 1 [[1 1 1 1] r r 1]] scl 0.1)
     ;(map-in [r 1 r [(rep 16 1)]] scl 0.1)
     ;(map-in [(rep 16 1)] scl 0.1)
                                        ;[(rep 32 1) r 1 (rep 8 1)]
     ; [1 [(rep 16 1)] r [1 1 1 1]]
    ;[(evr 4 1 (partition 1 (sfl (fll 16 [[1] [1] [r]]))))]
     :in-gate-select  [0]
     :in-amp [2.013]
     :in-note ; [ "fc1" "fg1" (rep 2 ["f f1" "fbb1"])]
    ; [ (rep 2 ["fc1" "fbb0"]) "fg1 " "fbb1"]
     ["fg1"]
     ;["f f1"]
     ;["f f0"]
     ;["fbb1"]
     ;(rep 1 [(rep 16 "ng2")])
     ;(rep 1 [(rep 16 "nbb2")])
     ;(rep 1 [(rep 16 "nf3")])
     :in-a [0.00125]
     :in-d [0.3]
     :in-s (acc [(range 0.5 2 0.1)])       ;[1.015]
     :in-r [0.275]; (slw 32 [(range 0.1 1 0.01)])
)

(pause! :gb2)

(play! :gb2)

(stp :gb2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;SETTIIIIIIIIII;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Alkaa UHEA:lla
;;;;

(do
  (add-sample "uh" (string-to-buffer "UHHHHHHHHAAAAAAAAAAAA"))

  (add-sample "ee" (string-to-buffer "EEEE"))

  (add-sample "aa" (string-to-buffer "AAAAAAAAAA"))

  (add-sample "oo" (string-to-buffer "OOOOOOOOOO"))

  (add-sample "uhea" (string-to-buffer "UHHHHHHHHEEEEEEEAAAAAAA"))

  (add-sample "hah" (string-to-buffer "hah"))

  (add-sample "bah" (string-to-buffer "bah"))

  (add-sample "beer" (string-to-buffer "beer"))

  (add-sample "fear" (string-to-buffer "fear"))

  (add-sample "ahh" (string-to-buffer "AHHHHH!"))

  (add-tts-sample "ks1"  "generalx2paradisedaqx2.txt" 5)

  (add-tts-sample "ks2"  "generalx2paradisedaqx2.txt" 5)

  (add-tts-sample "ks3"  "generalx2paradisedaqx2.txt" 5)

  (add-tts-sample "ks4"  "generalx2paradisedaqx2.txt" 5)

  (add-tts-sample "ks5"  "generalx2paradisedaqx2.txt" 5)

  (add-tts-sample "ks6"  "generalx2paradisedaqx2.txt" 5)

  (add-tts-sample "ks7"  "generalx2paradisedaqx2.txt" 5)

  (add-tts-sample "ks8"  "generalx2paradisedaqx2.txt" 5)

  (add-tts-sample "ks9"  "generalx2paradisedaqx2.txt" 5)

  )

(trg :uhsmp smp)

(pause! :uhsmp)

(trg :uhsmp smp
     :in-trg

     (evr 5 [(rep 32 "buh")] (evr 3 [r] (evr 4 acc (evr 8 (fn[x] (fst 16 x)) (slw 32 (fll 32 ["bks1" "bks2" "bks3" "bks4"]))))))

     :in-loop (evr 4 [1] (rep 32 [0]))
     :in-buf ":in-trg"
     :in-amp [0.75]
     :in-start-pos  (evr 4 [(range 0 1600 10)] (rep 32 [0]))
     :in-step (evr 4 (fst 4 [(range -0.5 2.5 0.25) 2 2 1]) (rep 32 [2]))
     )

(trg! :uhsmp :uhsmpe trg-fx-echo
      :in-amp (evr 6 [1] (rep 32 [0]))
      :in-decay-time [1.1]
      :in-delay-time [0.01])

(play! :uhsmp)

(pause! :uhsmp)

(sta)

(into () (vec  (->>  1 (rep 4) (vec) (rep  4))))

;;;;;;;
;;gb: tä mukaan
;;;;;;

(trg :gb2 vintage-bass)

(pause! :gb2)

(trg :gb2
     vintage-bass
     :in-trg (map-in [(rep 8 1)] scl 0.1)
     ;(map-in [1 [1 1 1 1] r [1 r r [1 1 1 r]]] scl 0.1)
     ;(map-in [1 [1 r 1 [1 1]] 1 [[1 1 1 1] r r 1]] scl 0.1)
     ;(map-in [r 1 r [(rep 16 1)]] scl 0.1)
     ;(map-in [(rep 16 1)] scl 0.1)
                                        ;[(rep 32 1) r 1 (rep 8 1)]
     ; [1 [(rep 16 1)] r [1 1 1 1]]
    ;[(evr 4 1 (partition 1 (sfl (fll 16 [[1] [1] [r]]))))]
     :in-gate-select  [0]
     :in-amp [2.013]
     :in-note [ "fc1" "fg1" (rep 2 ["f f1" "fbb1"])]
    [(rep 2 ["fc1" "fbb0"]) "fg1 " "fbb1"]
     ;["fg1"]
     ;["f f1"]
     ;["f f0"]
                                        ;
     ;["fbb1"]
     ;(rep 1 [(rep 16 "ng2")])
     ;(rep 1 [(rep 16 "nbb2")])
     ;(rep 1 [(rep 16 "nf3")])
     :in-a [0.00125]
     :in-d [0.3]
     :in-s (acc [(range 0.5 2 0.1)])       ;[1.015]
     :in-r [0.275]; (slw 32 [(range 0.1 1 0.01)])
)

(pause! :gb2)

(play! :gb2)

(volume! :gb2 0.5)

;;;;; setti2
;;;;Kick kurinaa sekaan
;;; uheat pois


(trg :kick kick)

(pause! :kick)

(trg :kick kick :in-trg   ; (rep 7 [(rep 16 1 )])
     ;[1 r 1 [1 1] 1 r 1 1 1 1 (acc [(rep 8 1)]) 1 1 1 1 [1 1 1 1]]
     ;[1 r r [(rep 16 1)]]
     ;[1 [(rep 8 1)] 1  r]
     ;[1 [1 1 1 1] [(rep 16 1)] r ]
     ;(rep 2 [(rep 8 1)])
     ;[1 1 1 r 1 1 1 [1 1 1 1]]

     (fst 4 [[1 2] r [3 4] r [5 6] r [7 8] 9])
     (fst 4 [[2 r 3 4] r [5 6 r 7] 1 [8 9 1 2 ] r [2 3] 4])
       (acc [(rep 64 1)])
       :in-f3  [ "fc1" "fg1" "f f1" "fbb1"]
        [ "fg1" "fc1" "f bb1" "ff1"]
        :in-f2 [100]
     :in-f1 (fll 32 [100 100])
     ;[ "fc2" "fg2" "ff2" "fbb2"]
     ;; [[ "fc3" "fg3" "ff3" "fbb3"]
     ;;  [ "fc3" "fg3" "ff3" "fbb3"]
     ;;  [ "fc4" "fg3" "ff2" "fbb1"]]
     :in-amp [0.25])

(volume! :kick 0.25)

(play! :kick)

(pause! :kick)

(stp :kick)

(trg :nh hat2)

(pause! :nh)

(trg :nh hat2
     :in-trg (rep 7 (fll 16 [0 2]))
     (fst 4 [[1 0] r [1 0] r [1 0] r [1 0] 1])
     ;(acc [(rep 64 1)])
     :in-attack [0.01 0.0001]
     :in-amp [1])

(volume! :nh 0.5)

(play! :nh)

(pause! :nh)

(pause! :kick)

;;;;Setti3-4
;;;Setti3;;;;;

(add-tts-sample "k1"  "generalx2paradisedaqx2.txt" 200)


(add-tts-sample "k2"  "generalx2paradisedaqx2.txt" 200)


(add-tts-sample "k3"  "generalx2paradisedaqx2.txt" 200)


(trg :ksmp smp)

(pause! :ksmp)

(trg :ksmp smp
     :in-trg [(rep 128 1)][1 1 1 1 1 1 1 [(rep 64 1)]] (rep 6 [r])
     :in-buf ["b k2"]
     :in-step [2] ;(slw 1 [(sir 32 2.5 1 32)]) ;[2]
     :in-loop [1]
     :in-start-pos  [(range 0 404040 5000)]
     :in-amp [1.0])

(pause! :ksmp)

(play! :ksmp)

;;;;;
;;;;;
;;Setti5;;;


(do
  (trg :tb303sn tb303)

  (pause! :tb303sn))

(trg :tb303sn
     tb303
     :in-trg
     (slw ["n c3" ["n c3" "n d3"]])
     (slw ["n e3" ["n c2"  "n c3"]])
     (slw 2 [["n e2" "n e3" r r] [r "n d3" "n e3" "n d2"]])
     (slw ["n d3" ["nd3" "nb2" "nf2" "ne3"]])

     :in-amp [1]
     :in-note  ":in-trg"
     :in-gate-select [1]
     :in-attack [0.001]
     :in-decay [0.019]
     :in-sustain [0.25]
     :in-release [5.73]
     :in-r [0.9]
     :in-cutoff [500]
     :in-wave  ;(rep 4 [0])
     (rep 4 [0]) ;(rep 2 [2])
     ;(rep 4 (fll 512 [0 1 2]))
     )

(pause! :tb303sn)

(play! :tb303sn)

;;;;;;;;;
;;Setti 6
;;;;;;;;;


(trg :bassd smp :in-amp [0])

(pause! :bassd)

( trg :bassd smp
      :in-trg
 (evr 16 [[(rep 8 1)] 1 1 [1 1 1 1]] (evr 8 [(rep 16 1)] (rep 16 [(rep 8 1)])))


 (sfl (evr 16 [[(rep 8 1)] 1 1 [1 1 1 1]] (evr 8 (fll 16 [1 r]) (rep 16 [(rep 8 1)]))))

     :in-step [2]; (evr 8 (fll 4 [2 -1.75 -1.75 -2]) (rep 16 [2]))
     :in-loop [0]
     :in-start-pos [0]
 :in-buf (fll 16 (sfl ["b bass23" r r "b sn1" r "b bass23" r   "b sn0"]))
  (fll 16 (sfl ["b bass23" r r "b sn1" r "b bass23" r   "b sn0"]))
 (fll 16 (sfl ["b bass23" r r "b sn1" r "b sn2" r   "b bd0"]))
  (fll 8 (sfl ["b bd1" r r "b sn1" r "b bass23" r   "b sn0"]))
                                        ;(evr 3 ["b bass20"] (rep 16 [ "b bd1" ])) ; (fll 8 [ "b bd1"  "b bass23"]) ;[ "b bd1" ])
)


(play! :bassd)

(pause! :bassd)

(trg! :bassd :bassde trg-fx-echo :in-decay-time [(/ (/ 1 0.5626)  2)]  :in-delay-time  [(/ (/ 1 0.5626)  50)] :in-amp (evr 16 [0.125 0.5 0.75 1 0.75 0.5 1 0.125] (rep 16 [0.125])))

(stp :bassd)

(sta)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




                                        ;Video
;(put-text-property 1 200 'face (cons 'foreground-color "red"))


(t/start "./b17.glsl" :width 1920 :height 1080 :cams [1] :videos ["../videos/jkl_fixed.mp4" "../videos/onnenp.mp4" "../videos/tietoisku_1_fixed.mp4" "../videos/spede_fixed.mp4" "../videos/sormileikit.mp4"])





                                        ;;;;;;;;;;;;;
                                        ;;Sormikäsi;;
                                        ;;;;video;;;;
                                        ;;;;;;;;;;;;;

(t/post-start-cam 3)


(t/set-cam-fps 3 1)

(t/bufferSection 0 0 9925)

(t/set-video-fixed 0 :fw)

(t/bufferSection 1 0 6460)

(t/set-video-fixed 1 :fw)

(t/bufferSection 2 0 1)

(t/set-video-fixed 2 :fw)

(t/set-video-fps 2 80)

(t/bufferSection 3 0 50900) ;51900

(t/set-video-fixed 3 :fw)

(t/set-video-fps 3 30)

(t/bufferSection 4 0 1)

(t/set-video-fixed 4 :fw)

(t/post-start-video "../videos/onnenp.mp4" 1)

(t/set-video-play 1)

(t/bufferSection 1 0 11800)

(t/set-video-fixed 1 :fw)


;(t/post-start-video "../videos/")


(def abm (audio-bus-monitor (get-out-bus :bassd)))


(def gbm (audio-bus-monitor (get-out-bus :gb2)))

(def uhbm (audio-bus-monitor (get-out-bus :uhsmp)))


(on-trigger (get-trigger-id :tick :in-trg)
            (fn [val]
              (let [obv  @abm]
                                        ;(println @uhbm)
                (t/set-dataArray-item 0 obv)
                (t/set-dataArray-item 7 @gbm)
                (t/set-dataArray-item 8 @uhbm)))
            :smp_obv)

(remove-event-handler :smp2_obv)



(on-trigger (get-trigger-val-id :tb303sn :in-trg)
            (fn [val]
              (let [obv  (float (* 10 val))]
                                        ;(t/set-fixed-buffer-index 2 :ff 10)
                ;(print obv)
                (t/set-dataArray-item 11 obv)))
            :tb303sn_obv)


(on-trigger (get-trigger-id :snare :in-trg) (fn [val]
                                             (t/set-fixed-buffer-index 2  :ff 50))
            :snaretrg)

(remove-event-handler :snaretrg)


(on-trigger (get-trigger-val-id :gb2 :in-trg)
            (fn [val]
              (t/set-fixed-buffer-index 2 :ff 50)
              ;(t/set-dataArray-item 3 (* val val))
              )
            :gb2trg)

(remove-event-handler :gbtrg)


(on-trigger (get-trigger-val-id :ks1 :in-trg) (fn [val]
                                           (t/set-dataArray-item 4 val))
            :ks1trg)

(remove-event-handler :kstrg)


(on-trigger (get-trigger-val-id :singlesmp :in-trg) (fn [val]
                                           (t/set-dataArray-item 5 val))
            :singlesmptrg)



(on-trigger (get-trigger-val-id :kick :in-f3) (fn [val]
                                           (t/set-dataArray-item 6 val))
            :kicktrg)

(remove-event-handler :kicktrg)


(on-trigger (get-trigger-val-id :nh :in-trg) (fn [val]
                                           (t/set-dataArray-item 9 val))
            :nhtrg)

(remove-event-handler :nhtrg)



(stp :kicktrg)

(t/stop)

(sta)


(t/toggle-recording "/dev/video1")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;Logo strings


(defn vt-logo2
  []
  (str "        ▒   ▒ ▒  ▒▒▒▓▒▒▓▒▒▒▒▒           ▒▓
       ▓ ▒ ▒▒▒▓▒▒▓▓▓▓▓▓▓▒▒▒▒▓▒▒▒▒▒     ▒ ▒ ▓ ▒▓▒
           ▓▒▓▓▓▓▓▓▓▓▒▓▓▒▒▒▒▒▓▒▓▓▓▒▓▒▒  ▒▓
       ▒▒▒▓▒▓▓▓▓▒▓▓▓▓▓▓▓▓▓▓▓▓▓▒▓▓▒▒▓▓▒▓▒
      ▒▒▒▒▒▒▓▓▓▒▓▓▓▒▒▒▒▓▒▓▓▒▒▓▒▓▒▓▓▓▒▒▒▒ ▒
       ▒▒ ▒▒▓▒▓▒▓▒▓▓▓▓▓▓▒▒▓▒▓▒▒▓▒▒▒▒▓ ▒▓ ▓▒
      ▒▒ ▓▓▒▓▓▓██████▓▓▓ ▓▒▓▓█▓▓███▒▓▒▒▒▒▒▒▒
      ▒▒▓▓▓▓▓ ▓█▓████▓▓▓▓▓▓▒▓██████▓▒▒▒▒▒▒▓▒
     ▒  ▓▓▒▓▓▓▓████▓█▓▓▒▓▓▒▓▒██████▓▓▓▒▒▓▓▓▒
        ▒ ▒▓▒▓▓▓██▒██▓▓▓▓▒▓▒▓▓▓█████▓▒▓▓▓▒▒▒
       ▒▒▒▒▓▒▓▓█████▓▓▓▓▒▒▒▓▒▒▓▓▓▓█▓▓▓▓▓▓▓▓▒
         ▒▒▒▓ ▒▒▓▒▓▓▒▓▓▓▒▓▒▓▒▓▒▒▒▓▓▒▓▓▓▓▓▓▓▒
      ▒▒   ▓▒▒▒ ▒▒▓▓▒▓▓▓▓▓▓▓▒▒▓▓▓▒▒▓▓▒▒▓▓▓▓
         ▒▒▒▒▒▒▒▒▓▓▓▓▓▓▓▓▓▓▓  ▓▓▒▒▓▓▒▓▒▒▓▓▒▒▒▒▓
         ▒▒▒▒▓▓▓▓▓▓▓▓▓▒▓▒▓▓▓▒▒▓▓▓▒▒▒▓▒▓▒▓▓▒
       ▒▒ ▒▓▓▓▓▓▓▓▓▓▓▓▓▒▒▓▓▒▓▓▓▓▓▓▓▒▓▓▒▓▓▒▓
       ▒▒▒▒▓▓▓▓▓▓▓▓▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▒▓▓▓▓▓▒▓▒ ▒▒▒
    ▒  ▒▒▓▒▓▓▓▓██▓▒▒▓▓▓▓▓▓▓▓▓███▓██▒▓▓▓▓▓▓▓▒▒█▒▒
       ▓▓▓▒▓█████████████████████████▓▓▓▒▒▓
       ▒▓▓▒▓█████████████████████████▓▓▒▒▓
       ▒▒▒▒▓█████████████████▓▓▓▓▓▓▓▓▓▓▓▒▒
 ▒      ▒▓▒▒▒▓▓▓▓█▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
 █  ▒  ▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓█▓▓▒▓▒▓▓▓▓▓▓▓▓
      ▒ ▒▒▓▓▓▓▓▓▓▓▓▓▓▓▓▓█▓▓▓█▓▒▒▒▓▓▓▓▓▓▓▓▓▒  "))

(vt-logo2)


(t/stop)
