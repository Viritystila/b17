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

(set-pattern-delay 0.99)


                                        ;;;;;;;;;;;;;
                                        ;Fingerhands;
                                        ;;;;Sound;;;;
                                        ;;;;;;;;;;;;;
(trg :smp smp2)

(pause! :smp)

(trg :smp smp2
     :in-trg1  [1 r r  r]
     [1 r r  [1 1 r r]]
     [[[1 1] r] [1 1] r  [1 1 r r]]
     [r  [1 1 r r] 1 r]
     [[1 1 r r] r r [(rep 4 1 )]]
     [1 r r [1 1 r r]]
     [[r r 1 1 r r r r] r]
     [[(rep 4 1)] r 1 [1 r] ]
     :in-step1 [2]
     :in-loop1 [0]
     :in-start-pos1 [0]
     :in-buf1 [ "b bd1" ] ; (fll 8 [ "b bd1"  "b bass23"]) ;[ "b bd1" ]
     :in-trg2
     [r [r  1] 1 1]
     [r 1 r [r r 1 1]]
     [r [1 1] 1 [r 1]]
     [[(rep 4 1)]  r [1 1 r r] r]
     [[1 1 r r] r r [(rep 32 2)]]
     [1 r 1 [(rep 48 1)]]
     [[1 1 r r] r r [(rep 32 1)] r r [(rep 16 1)] r]
     ;[1 r 1 [(rep 16 2)]]
     :in-step2 [1]; [1 [1 1] 1.5 1.5]     ; [1.5] [1]       ;":in-trg2"
     :in-loop2 [0]
     :in-start-pos2 [0]
     :in-buf2  (fll 32 ["b sn1" "bsn4"])
     :in-amp2 [0.15])

(play! :smp)

(pause! :smp)

(stp :smp)

(trg! :smp :smpe trg-fx-echo :in-decay-time [(/ (/ 1 0.5626)  2)]  :in-delay-time  [(/ (/ 1 0.5626)  50)] :in-amp [0.15])

(sta)

(trg :tb303sn tb303)

(pause! :tb303sn)

(trg :tb303sn
     tb303
     :in-trg  ;["n d1" r ["n c2" "n d3"]  "n d3"]
     ;["n d1" r "n c2"  "n d3"]
     ;["n d1" r ["n c2" "n d3"]  [r "n d3" r "n d1"]]
     ;[["n d1" "n d2"] r  ["n c2" "n e2" "n e1" "n e2"]  ["n e2" "n d3"]]

     ["n c3" ["n c3" "n d3"]]
     ["n e3" ["n c2"  "n c3"]]
     [["n e2" "n e3" r r] [r "n d3" "n e3" "n d2"]]
     ["n d3" ["nd3" "nc2" "ne2" "ne3"]]
     ;[["n d1" "n d2"] ["n c2" "n e2" "n e1" "n e2"]]

     ;["n d1" "ne0" ["n c1" "n d0"]  ["n d0" "n e1"]]
     ;["n d1" "n c0" ["n c1" "n d0"]  ["n d0" "n e1" "n d2" "n c2"]]
     :in-amp [1]
     :in-note  ":in-trg"
     :in-gate-select [1]
     :in-attack [0.001]
     :in-decay [0.019]
     :in-sustain [0.25]
     :in-release [1.73]
     :in-r [0.9]
     :in-cutoff [700]
     :in-wave  ;(rep 4 [0])
     (rep 4 [2])
     ;(rep 4 (fll 512 [0 1 2]))
     )

(play! :tb303sn)

(pause! :tb303sn)

(volume! :tb303sn 0.5)

(trg! :tb303sn :tb303e trg-fx-echo :in-decay-time [0.125]  :in-delay-time [0.1] :in-amp [1])

(stp :tb303sn)

(stp :tb303e)

(trg :hhsmp smp :in-amp [0])

(pause! :hhsmp)

(trg :hhsmp smp
     :in-trg  (rep 1 [r] )
     [r r r [(rep 16 1)]]
     [r]
     [1  [(rep 4 1)] 1 [(rep 8 1)]]
     [(rep 16 1)]
     :in-step  [2]; (fst 16 [(range -3 3 0.01)])
     :in-loop [0] ;(rep 3 [0])
     :in-amp [0.5]
     :in-buf (fll 128 ["b co3" "b cc0"]) )


(volume! :hhsmp 0.25)

(stp :hhsmp)

(trg! :hhsmp :hhsmppc trg-fx-pitch-shift :in-pitch-ratio  (slw 4 [(range 0.5 1.5 0.01)])
      )

(play! :hhsmp)

(stp :hhsmppc)

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
     [(rep 4 1)]

                                        ;[r [r  1] 1 1]
     ;[r 1 r [r r 1 1]]
     ;[r [1 1] 1 [r 1]]
     ;[[(rep 4 1)]  r [1 1 r r] r]
     :in-loop [0]
     :in-buf ["b uhea"]
     ["b uh"] ["b oo"] (fst 16 ["b aa" "b ee"])
     :in-amp [0.5]
     :in-step [2]; (fst 32 [(range 1.5 2.5 0.25)])
     )

(play! :uhsmp)

(volume! :uhsmp 0.5)

(stp :uhsmp)

(trg :tick ping)

(pause! :tick)

(trg :tick ping :in-amp [0] :in-trg [(rep 60 1)])

(play! :tick)

(stp :tick)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(trg :ks1 ks1)

(pause! :ks1)

(trg :ks1
     ks1
     :in-trg  [1 1 [1 1] 1]
     [r]
     [1 r [1 [1 1]] [r 1]]
     [r]

     ;(acc (fst 4 [1 1 [1 1] 1]))
     ;(rep 1 [r])
     ;(fst 64 [1 r [1 [1 1]] [r 1]])
     ;(rep 1 [r])
     :in-dur [1]
     :in-amp [1]
     :in-note
     (rep 1 ["n f#3"])
     ;(rep 1 ["n d3"])
     ;(rep 1 ["n a3"])
     ;(rep 1 ["n c#4"])
     ;(rep 1 ["n f#2"])
     ;(rep 1 ["n d2"])
     ;(rep 1 ["n a2"])
     ;(rep 1 ["n c#3"])
     :in-decay [0.9]; [(range 0.01 1 0.01)]
     :in-coef [0.01] ;[(range 0.01 0.9 0.01)]
     )

(play! :ks1)

(volume! :ks1 1)

(trg! :ks1 :ks1f trg-fx-feedback :in-delay-time [0.25] :in-decay-time [0.25])

(stp :ks1)

(println (map find-note-name (chord-degree :ii :d4 :melodic-minor)))

(trg :op overpad)

(pause! :op)

(trg :op overpad
     :in-trg [1 1 1 1]
                                        ;[r]
     ;(map-in [(rep 8 [1 r r 1])] scl 0.5)
     :in-note  (rep 1 ["n f#2"])
     (rep 1 ["n d2"])
     (rep 1 ["n a2"])
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

(trg! :op :ope trg-fx-echo :in-decay-time [0.25]  :in-delay-time [0.001] :in-amp [0.05])

(stp :ope)

(stp :op)


(add-tts-sample "k1"  "generalx2paradisedaqx2.txt" 200)


(add-tts-sample "k2"  "generalx2paradisedaqx2.txt" 200)


(add-tts-sample "k3"  "generalx2paradisedaqx2.txt" 200)


(trg :ksmp smp)

(pause! :ksmp)

(trg :ksmp smp
     :in-trg [r] ;[(rep 128 1)][1 1 1 1 1 1 1 [(rep 64 1)]] (rep 7 [r])
     :in-buf ["b k3"]
     :in-step [2] ;(slw 1 [(sir 32 2.5 1 32)]) ;[2]
     :in-loop [1]
     :in-start-pos (slw 4  [(range 0 404040 5000)])
     :in-amp [1.0])


(play! :ksmp)

(stp :ksmp)

(lss)

(sta)
                                        ;Video
;(put-text-property 1 200 'face (cons 'foreground-color "red"))


(t/start "./b17.glsl" :width 1920 :height 1080 :cams [0] :videos ["../videos/jkl_fixed.mp4" "../videos/uni_fixed.mp4" "../videos/tietoisku_1_fixed.mp4" "../videos/spede_fixed.mp4" "../videos/sormileikit.mp4"])


                                        ;;;;;;;;;;;;;
                                        ;Fingerhands;
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

(t/bufferSection 3 0 51900)

(t/set-video-fixed 3 :fw)

(t/set-video-fps 3 30)

(t/bufferSection 4 0 1)

(t/set-video-fixed 4 :fw)


(def abm (audio-bus-monitor (get-out-bus :smp)))

(on-trigger (get-trigger-id :tick :in-trg) (fn [val]
                                             (let [obv  @abm]
                                               ;(println obv)
                                               (t/set-dataArray-item 0 obv)))
            :smp_obv)

(remove-event-handler :smp2_obv)



(on-trigger (get-trigger-val-id :tb303sn :in-trg) (fn [val]
                                                    (let [obv  (int (* 10 val))]
                                              (t/set-fixed-buffer-index 2 :ff 10)
                                               (t/set-dataArray-item 1 obv)))
            :tb303sn_obv)


(on-trigger (get-trigger-id :smp :in-trg1) (fn [val]
                                             (t/set-fixed-buffer-index 3  :ff 0))
            :smptrg)

(t/stop)

(sta)
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
