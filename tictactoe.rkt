#lang racket

(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

(define a1 0)
(define a2 0)
(define a3 0)
(define a4 0)

(define T1 "")
(define T2 "")
(define T3 "")
(define T4 "")
(define T5 "")
(define T6 "")
(define T7 "")
(define T8 "")
(define T9 "")

(define at1 "no")
(define at2 "no")
(define at3 "no")
(define at4 "no")
(define at5 "no")
(define at6 "no")
(define at7 "no")
(define at8 "no")
(define at9 "no")

(define t1 "no")
(define t2 "no")
(define t3 "no")
(define t4 "no")
(define t5 "no")
(define t6 "no")
(define t7 "no")
(define t8 "no")
(define t9 "no")

(define ai1 "")
(define ai2 "")
(define ai3 "")
(define ai4 "")

(define scene (square 500 0 "white"))
(define line1 (line 0 450 "black"))
(define line2 (line 0 450 "black"))
(define line3 (line 450 0 "black"))
(define line4 (line 450 0 "black"))
(define cross (overlay (line 150 150 "red") (line 150 -150 "red")))
(define circ (circle 75 "outline" "blue"))

(define grid
  (underlay/align/offset "middle" "top"
   (underlay/align/offset "middle" "top"
    (underlay/align/offset "left" "middle"
     (underlay/align/offset "left" "middle" scene 325 0 line1)
     175 0 line2)
    0 175 line3)
   0 325 line4))

(define (randomw a b c d e)
  (define rn (random 1 10))
  (cond
    [(not (or (= rn a)(= rn b)(= rn c)(= rn d)(= rn e))) rn]
    [else (randomw a b c d e)]))

(define (d x)
 (cond
   [(or (and (string=? t1 "yes")(string=? t4 "yes")(string=? t7 "yes"))
        (and (string=? t2 "yes")(string=? t5 "yes")(string=? t8 "yes"))
        (and (string=? t3 "yes")(string=? t6 "yes")(string=? t9 "yes"))
        (and (string=? t1 "yes")(string=? t2 "yes")(string=? t3 "yes"))
        (and (string=? t4 "yes")(string=? t5 "yes")(string=? t6 "yes"))
        (and (string=? t7 "yes")(string=? t8 "yes")(string=? t9 "yes"))
        (and (string=? t1 "yes")(string=? t5 "yes")(string=? t9 "yes"))
        (and (string=? t3 "yes")(string=? t5 "yes")(string=? t7 "yes"))) (underlay/align "middle" "middle" (circle 250 0 "red") (text "You win!" 100 "indigo"))]
   [(or (and (or (= a1 1)(= a2 1)(= a3 1)(= a4 1))(or (= a1 4)(= a2 4)(= a3 4)(= a4 4))(or (= a1 7)(= a2 7)(= a3 7)(= a4 7)))
        (and (or (= a1 2)(= a2 2)(= a3 2)(= a4 2))(or (= a1 5)(= a2 5)(= a3 5)(= a4 5))(or (= a1 8)(= a2 8)(= a3 8)(= a4 8)))
        (and (or (= a1 3)(= a2 3)(= a3 3)(= a4 3))(or (= a1 6)(= a2 6)(= a3 6)(= a4 6))(or (= a1 9)(= a2 9)(= a3 9)(= a4 9)))
        (and (or (= a1 1)(= a2 1)(= a3 1)(= a4 1))(or (= a1 2)(= a2 2)(= a3 2)(= a4 2))(or (= a1 3)(= a2 3)(= a3 3)(= a4 3)))
        (and (or (= a1 4)(= a2 4)(= a3 4)(= a4 4))(or (= a1 5)(= a2 5)(= a3 5)(= a4 5))(or (= a1 6)(= a2 6)(= a3 6)(= a4 6)))
        (and (or (= a1 7)(= a2 7)(= a3 7)(= a4 7))(or (= a1 8)(= a2 8)(= a3 8)(= a4 8))(or (= a1 9)(= a2 9)(= a3 9)(= a4 9)))
        (and (or (= a1 1)(= a2 1)(= a3 1)(= a4 1))(or (= a1 5)(= a2 5)(= a3 5)(= a4 5))(or (= a1 9)(= a2 9)(= a3 9)(= a4 9)))
        (and (or (= a1 3)(= a2 3)(= a3 3)(= a4 3))(or (= a1 5)(= a2 5)(= a3 5)(= a4 5))(or (= a1 7)(= a2 7)(= a3 7)(= a4 7)))) (underlay/align "middle" "middle" (circle 250 0 "red")
                                                                                                                                               (text "  You've just lost to a computer.\n          There isn't even an\n          artificial intelligence\n        making the decisions.\n            You literally lost\n         to random numbers.\n   You imbecile. You disgrace.\n                  Pathetic." 35 "indigo"))]
   [(= x 5) (underlay/align "middle" "middle" (circle 250 0 "red") (text "Tie?" 100 "indigo"))]
   [else (begin (cond
                  [(and (string? ai1) (= x 1)) (cond
                                                 [(or (string=? t1 "yes")(string=? t3 "yes")(string=? t7 "yes")(string=? t9 "yes")) (set! ai1 5)]
                                                 [else (set! ai1 (randomw 2 4 5 6 8))])]
                  [(and (string? ai2) (= x 2)) (cond
                                                 [(and (string=? t1 "no")(string=? t4 "yes")(string=? t7 "yes")(string=? at1 "no")) (set! ai2 1)]
                                                 [(and (string=? t1 "yes")(string=? t4 "no")(string=? t7 "yes")(string=? at4 "no")) (set! ai2 4)]
                                                 [(and (string=? t1 "yes")(string=? t4 "yes")(string=? t7 "no")(string=? at7 "no")) (set! ai2 7)]
                                                 [(and (string=? t2 "no")(string=? t5 "yes")(string=? t8 "yes")(string=? at2 "no")) (set! ai2 2)]
                                                 [(and (string=? t2 "yes")(string=? t5 "no")(string=? t8 "yes")(string=? at5 "no")) (set! ai2 5)]
                                                 [(and (string=? t2 "yes")(string=? t5 "yes")(string=? t8 "no")(string=? at8 "no")) (set! ai2 8)]
                                                 [(and (string=? t3 "no")(string=? t6 "yes")(string=? t9 "yes")(string=? at3 "no")) (set! ai2 3)]
                                                 [(and (string=? t3 "yes")(string=? t6 "no")(string=? t9 "yes")(string=? at6 "no")) (set! ai2 6)]
                                                 [(and (string=? t3 "yes")(string=? t6 "yes")(string=? t9 "no")(string=? at9 "no")) (set! ai2 9)]
                                                 [(and (string=? t1 "no")(string=? t2 "yes")(string=? t3 "yes")(string=? at1 "no")) (set! ai2 1)]
                                                 [(and (string=? t1 "yes")(string=? t2 "no")(string=? t3 "yes")(string=? at2 "no")) (set! ai2 2)]
                                                 [(and (string=? t1 "yes")(string=? t2 "yes")(string=? t3 "no")(string=? at3 "no")) (set! ai2 3)]
                                                 [(and (string=? t4 "no")(string=? t5 "yes")(string=? t6 "yes")(string=? at4 "no")) (set! ai2 4)]
                                                 [(and (string=? t4 "yes")(string=? t5 "no")(string=? t6 "yes")(string=? at5 "no")) (set! ai2 5)]
                                                 [(and (string=? t4 "yes")(string=? t5 "yes")(string=? t6 "no")(string=? at6 "no")) (set! ai2 6)]
                                                 [(and (string=? t7 "no")(string=? t8 "yes")(string=? t9 "yes")(string=? at7 "no")) (set! ai2 7)]
                                                 [(and (string=? t7 "yes")(string=? t8 "no")(string=? t9 "yes")(string=? at8 "no")) (set! ai2 8)]
                                                 [(and (string=? t7 "yes")(string=? t8 "yes")(string=? t9 "no")(string=? at9 "no")) (set! ai2 9)]
                                                 [(and (string=? t1 "no")(string=? t5 "yes")(string=? t9 "yes")(string=? at1 "no")) (set! ai2 1)]
                                                 [(and (string=? t1 "yes")(string=? t5 "no")(string=? t9 "yes")(string=? at5 "no")) (set! ai2 5)]
                                                 [(and (string=? t1 "yes")(string=? t5 "yes")(string=? t9 "no")(string=? at9 "no")) (set! ai2 9)]
                                                 [(and (string=? t3 "no")(string=? t5 "yes")(string=? t7 "yes")(string=? at3 "no")) (set! ai2 3)]
                                                 [(and (string=? t3 "yes")(string=? t5 "no")(string=? t7 "yes")(string=? at5 "no")) (set! ai2 5)]
                                                 [(and (string=? t3 "yes")(string=? t5 "yes")(string=? t7 "no")(string=? at7 "no")) (set! ai2 7)]
                                                 [(string=? at1 "yes") (set! ai2 (randomw 6 8 0 0 0))]
                                                 [(string=? at2 "yes") (set! ai2 (randomw 4 6 7 9 0))]
                                                 [(string=? at3 "yes") (set! ai2 (randomw 4 8 0 0 0))]
                                                 [(string=? at4 "yes") (set! ai2 (randomw 2 3 8 9 0))]
                                                 [(string=? at5 "yes") (set! ai2 (randomw 0 0 0 0 0))]
                                                 [(string=? at6 "yes") (set! ai2 (randomw 1 2 7 8 0))]
                                                 [(string=? at7 "yes") (set! ai2 (randomw 2 6 0 0 0))]
                                                 [(string=? at8 "yes") (set! ai2 (randomw 1 3 4 6 0))]
                                                 [(string=? at9 "yes") (set! ai2 (randomw 2 4 0 0 0))]
                                                 [else (set! ai2 (random 1 10))])]
                  [(and (string? ai3) (= x 3)) (cond
                                                 [(and (string=? at1 "no")(string=? at4 "yes")(string=? at7 "yes")(string=? t1 "no")) (set! ai3 1)]
                                                 [(and (string=? at1 "yes")(string=? at4 "no")(string=? at7 "yes")(string=? t4 "no")) (set! ai3 4)]
                                                 [(and (string=? at1 "yes")(string=? at4 "yes")(string=? at7 "no")(string=? t7 "no")) (set! ai3 7)]
                                                 [(and (string=? at2 "no")(string=? at5 "yes")(string=? at8 "yes")(string=? t2 "no")) (set! ai3 2)]
                                                 [(and (string=? at2 "yes")(string=? at5 "no")(string=? at8 "yes")(string=? t5 "no")) (set! ai3 5)]
                                                 [(and (string=? at2 "yes")(string=? at5 "yes")(string=? at8 "no")(string=? t8 "no")) (set! ai3 8)]
                                                 [(and (string=? at3 "no")(string=? at6 "yes")(string=? at9 "yes")(string=? t3 "no")) (set! ai3 3)]
                                                 [(and (string=? at3 "yes")(string=? at6 "no")(string=? at9 "yes")(string=? t6 "no")) (set! ai3 6)]
                                                 [(and (string=? at3 "yes")(string=? at6 "yes")(string=? at9 "no")(string=? t9 "no")) (set! ai3 9)]
                                                 [(and (string=? at1 "no")(string=? at2 "yes")(string=? at3 "yes")(string=? t1 "no")) (set! ai3 1)]
                                                 [(and (string=? at1 "yes")(string=? at2 "no")(string=? at3 "yes")(string=? t2 "no")) (set! ai3 2)]
                                                 [(and (string=? at1 "yes")(string=? at2 "yes")(string=? at3 "no")(string=? t3 "no")) (set! ai3 3)]
                                                 [(and (string=? at4 "no")(string=? at5 "yes")(string=? at6 "yes")(string=? t4 "no")) (set! ai3 4)]
                                                 [(and (string=? at4 "yes")(string=? at5 "no")(string=? at6 "yes")(string=? t5 "no")) (set! ai3 5)]
                                                 [(and (string=? at4 "yes")(string=? at5 "yes")(string=? at6 "no")(string=? t6 "no")) (set! ai3 6)]
                                                 [(and (string=? at7 "no")(string=? at8 "yes")(string=? at9 "yes")(string=? t7 "no")) (set! ai3 7)]
                                                 [(and (string=? at7 "yes")(string=? at8 "no")(string=? at9 "yes")(string=? t8 "no")) (set! ai3 8)]
                                                 [(and (string=? at7 "yes")(string=? at8 "yes")(string=? at9 "no")(string=? t9 "no")) (set! ai3 9)]
                                                 [(and (string=? at1 "no")(string=? at5 "yes")(string=? at9 "yes")(string=? t1 "no")) (set! ai3 1)]
                                                 [(and (string=? at1 "yes")(string=? at5 "no")(string=? at9 "yes")(string=? t5 "no")) (set! ai3 5)]
                                                 [(and (string=? at1 "yes")(string=? at5 "yes")(string=? at9 "no")(string=? t9 "no")) (set! ai3 9)]
                                                 [(and (string=? at3 "no")(string=? at5 "yes")(string=? at7 "yes")(string=? t3 "no")) (set! ai3 3)]
                                                 [(and (string=? at3 "yes")(string=? at5 "no")(string=? at7 "yes")(string=? t5 "no")) (set! ai3 5)]
                                                 [(and (string=? at3 "yes")(string=? at5 "yes")(string=? at7 "no")(string=? t7 "no")) (set! ai3 7)]
                                                 [(and (string=? t1 "no")(string=? t4 "yes")(string=? t7 "yes")(string=? at1 "no")) (set! ai3 1)]
                                                 [(and (string=? t1 "yes")(string=? t4 "no")(string=? t7 "yes")(string=? at4 "no")) (set! ai3 4)]
                                                 [(and (string=? t1 "yes")(string=? t4 "yes")(string=? t7 "no")(string=? at7 "no")) (set! ai3 7)]
                                                 [(and (string=? t2 "no")(string=? t5 "yes")(string=? t8 "yes")(string=? at2 "no")) (set! ai3 2)]
                                                 [(and (string=? t2 "yes")(string=? t5 "no")(string=? t8 "yes")(string=? at5 "no")) (set! ai3 5)]
                                                 [(and (string=? t2 "yes")(string=? t5 "yes")(string=? t8 "no")(string=? at8 "no")) (set! ai3 8)]
                                                 [(and (string=? t3 "no")(string=? t6 "yes")(string=? t9 "yes")(string=? at3 "no")) (set! ai3 3)]
                                                 [(and (string=? t3 "yes")(string=? t6 "no")(string=? t9 "yes")(string=? at6 "no")) (set! ai3 6)]
                                                 [(and (string=? t3 "yes")(string=? t6 "yes")(string=? t9 "no")(string=? at9 "no")) (set! ai3 9)]
                                                 [(and (string=? t1 "no")(string=? t2 "yes")(string=? t3 "yes")(string=? at1 "no")) (set! ai3 1)]
                                                 [(and (string=? t1 "yes")(string=? t2 "no")(string=? t3 "yes")(string=? at2 "no")) (set! ai3 2)]
                                                 [(and (string=? t1 "yes")(string=? t2 "yes")(string=? t3 "no")(string=? at3 "no")) (set! ai3 3)]
                                                 [(and (string=? t4 "no")(string=? t5 "yes")(string=? t6 "yes")(string=? at4 "no")) (set! ai3 4)]
                                                 [(and (string=? t4 "yes")(string=? t5 "no")(string=? t6 "yes")(string=? at5 "no")) (set! ai3 5)]
                                                 [(and (string=? t4 "yes")(string=? t5 "yes")(string=? t6 "no")(string=? at6 "no")) (set! ai3 6)]
                                                 [(and (string=? t7 "no")(string=? t8 "yes")(string=? t9 "yes")(string=? at7 "no")) (set! ai3 7)]
                                                 [(and (string=? t7 "yes")(string=? t8 "no")(string=? t9 "yes")(string=? at8 "no")) (set! ai3 8)]
                                                 [(and (string=? t7 "yes")(string=? t8 "yes")(string=? t9 "no")(string=? at9 "no")) (set! ai3 9)]
                                                 [(and (string=? t1 "no")(string=? t5 "yes")(string=? t9 "yes")(string=? at1 "no")) (set! ai3 1)]
                                                 [(and (string=? t1 "yes")(string=? t5 "no")(string=? t9 "yes")(string=? at5 "no")) (set! ai3 5)]
                                                 [(and (string=? t1 "yes")(string=? t5 "yes")(string=? t9 "no")(string=? at9 "no")) (set! ai3 9)]
                                                 [(and (string=? t3 "no")(string=? t5 "yes")(string=? t7 "yes")(string=? at3 "no")) (set! ai3 3)]
                                                 [(and (string=? t3 "yes")(string=? t5 "no")(string=? t7 "yes")(string=? at5 "no")) (set! ai3 5)]
                                                 [(and (string=? t3 "yes")(string=? t5 "yes")(string=? t7 "no")(string=? at7 "no")) (set! ai3 7)]
                                                 [else (set! ai3 (random 1 10))])]
                  [(and (string? ai4) (= x 4)) (cond
                                                 [(and (string=? at1 "no")(string=? at4 "yes")(string=? at7 "yes")(string=? t1 "no")) (set! ai4 1)]
                                                 [(and (string=? at1 "yes")(string=? at4 "no")(string=? at7 "yes")(string=? t4 "no")) (set! ai4 4)]
                                                 [(and (string=? at1 "yes")(string=? at4 "yes")(string=? at7 "no")(string=? t7 "no")) (set! ai4 7)]
                                                 [(and (string=? at2 "no")(string=? at5 "yes")(string=? at8 "yes")(string=? t2 "no")) (set! ai4 2)]
                                                 [(and (string=? at2 "yes")(string=? at5 "no")(string=? at8 "yes")(string=? t5 "no")) (set! ai4 5)]
                                                 [(and (string=? at2 "yes")(string=? at5 "yes")(string=? at8 "no")(string=? t8 "no")) (set! ai4 8)]
                                                 [(and (string=? at3 "no")(string=? at6 "yes")(string=? at9 "yes")(string=? t3 "no")) (set! ai4 3)]
                                                 [(and (string=? at3 "yes")(string=? at6 "no")(string=? at9 "yes")(string=? t6 "no")) (set! ai4 6)]
                                                 [(and (string=? at3 "yes")(string=? at6 "yes")(string=? at9 "no")(string=? t9 "no")) (set! ai4 9)]
                                                 [(and (string=? at1 "no")(string=? at2 "yes")(string=? at3 "yes")(string=? t1 "no")) (set! ai4 1)]
                                                 [(and (string=? at1 "yes")(string=? at2 "no")(string=? at3 "yes")(string=? t2 "no")) (set! ai4 2)]
                                                 [(and (string=? at1 "yes")(string=? at2 "yes")(string=? at3 "no")(string=? t3 "no")) (set! ai4 3)]
                                                 [(and (string=? at4 "no")(string=? at5 "yes")(string=? at6 "yes")(string=? t4 "no")) (set! ai4 4)]
                                                 [(and (string=? at4 "yes")(string=? at5 "no")(string=? at6 "yes")(string=? t5 "no")) (set! ai4 5)]
                                                 [(and (string=? at4 "yes")(string=? at5 "yes")(string=? at6 "no")(string=? t6 "no")) (set! ai4 6)]
                                                 [(and (string=? at7 "no")(string=? at8 "yes")(string=? at9 "yes")(string=? t7 "no")) (set! ai4 7)]
                                                 [(and (string=? at7 "yes")(string=? at8 "no")(string=? at9 "yes")(string=? t8 "no")) (set! ai4 8)]
                                                 [(and (string=? at7 "yes")(string=? at8 "yes")(string=? at9 "no")(string=? t9 "no")) (set! ai4 9)]
                                                 [(and (string=? at1 "no")(string=? at5 "yes")(string=? at9 "yes")(string=? t1 "no")) (set! ai4 1)]
                                                 [(and (string=? at1 "yes")(string=? at5 "no")(string=? at9 "yes")(string=? t5 "no")) (set! ai4 5)]
                                                 [(and (string=? at1 "yes")(string=? at5 "yes")(string=? at9 "no")(string=? t9 "no")) (set! ai4 9)]
                                                 [(and (string=? at3 "no")(string=? at5 "yes")(string=? at7 "yes")(string=? t3 "no")) (set! ai4 3)]
                                                 [(and (string=? at3 "yes")(string=? at5 "no")(string=? at7 "yes")(string=? t5 "no")) (set! ai4 5)]
                                                 [(and (string=? at3 "yes")(string=? at5 "yes")(string=? at7 "no")(string=? t7 "no")) (set! ai4 7)]
                                                 [(and (string=? t1 "no")(string=? t4 "yes")(string=? t7 "yes")(string=? at1 "no")) (set! ai4 1)]
                                                 [(and (string=? t1 "yes")(string=? t4 "no")(string=? t7 "yes")(string=? at4 "no")) (set! ai4 4)]
                                                 [(and (string=? t1 "yes")(string=? t4 "yes")(string=? t7 "no")(string=? at7 "no")) (set! ai4 7)]
                                                 [(and (string=? t2 "no")(string=? t5 "yes")(string=? t8 "yes")(string=? at2 "no")) (set! ai4 2)]
                                                 [(and (string=? t2 "yes")(string=? t5 "no")(string=? t8 "yes")(string=? at5 "no")) (set! ai4 5)]
                                                 [(and (string=? t2 "yes")(string=? t5 "yes")(string=? t8 "no")(string=? at8 "no")) (set! ai4 8)]
                                                 [(and (string=? t3 "no")(string=? t6 "yes")(string=? t9 "yes")(string=? at3 "no")) (set! ai4 3)]
                                                 [(and (string=? t3 "yes")(string=? t6 "no")(string=? t9 "yes")(string=? at6 "no")) (set! ai4 6)]
                                                 [(and (string=? t3 "yes")(string=? t6 "yes")(string=? t9 "no")(string=? at9 "no")) (set! ai4 9)]
                                                 [(and (string=? t1 "no")(string=? t2 "yes")(string=? t3 "yes")(string=? at1 "no")) (set! ai4 1)]
                                                 [(and (string=? t1 "yes")(string=? t2 "no")(string=? t3 "yes")(string=? at2 "no")) (set! ai4 2)]
                                                 [(and (string=? t1 "yes")(string=? t2 "yes")(string=? t3 "no")(string=? at3 "no")) (set! ai4 3)]
                                                 [(and (string=? t4 "no")(string=? t5 "yes")(string=? t6 "yes")(string=? at4 "no")) (set! ai4 4)]
                                                 [(and (string=? t4 "yes")(string=? t5 "no")(string=? t6 "yes")(string=? at5 "no")) (set! ai4 5)]
                                                 [(and (string=? t4 "yes")(string=? t5 "yes")(string=? t6 "no")(string=? at6 "no")) (set! ai4 6)]
                                                 [(and (string=? t7 "no")(string=? t8 "yes")(string=? t9 "yes")(string=? at7 "no")) (set! ai4 7)]
                                                 [(and (string=? t7 "yes")(string=? t8 "no")(string=? t9 "yes")(string=? at8 "no")) (set! ai4 8)]
                                                 [(and (string=? t7 "yes")(string=? t8 "yes")(string=? t9 "no")(string=? at9 "no")) (set! ai4 9)]
                                                 [(and (string=? t1 "no")(string=? t5 "yes")(string=? t9 "yes")(string=? at1 "no")) (set! ai4 1)]
                                                 [(and (string=? t1 "yes")(string=? t5 "no")(string=? t9 "yes")(string=? at5 "no")) (set! ai4 5)]
                                                 [(and (string=? t1 "yes")(string=? t5 "yes")(string=? t9 "no")(string=? at9 "no")) (set! ai4 9)]
                                                 [(and (string=? t3 "no")(string=? t5 "yes")(string=? t7 "yes")(string=? at3 "no")) (set! ai4 3)]
                                                 [(and (string=? t3 "yes")(string=? t5 "no")(string=? t7 "yes")(string=? at5 "no")) (set! ai4 5)]
                                                 [(and (string=? t3 "yes")(string=? t5 "yes")(string=? t7 "no")(string=? at7 "no")) (set! ai4 7)]
                                                 [else (set! ai4 (random 1 10))])])
                (overlay
                 (cond
                   [(string=? t1 "yes") (underlay/offset grid -150 -150 cross)]
                   [else grid])
                 (cond
                   [(string=? t2 "yes") (underlay/offset grid 0 -150 cross)]
                   [else grid])
                 (cond
                   [(string=? t3 "yes") (underlay/offset grid 150 -150 cross)]
                   [else grid])
                 (cond
                   [(string=? t4 "yes") (underlay/offset grid -150 0 cross)]
                   [else grid])
                 (cond
                   [(string=? t5 "yes") (underlay/offset grid 0 0 cross)]
                   [else grid])
                 (cond
                   [(string=? t6 "yes") (underlay/offset grid 150 0 cross)]
                   [else grid])
                 (cond
                   [(string=? t7 "yes") (underlay/offset grid -150 150 cross)]
                   [else grid])
                 (cond
                   [(string=? t8 "yes") (underlay/offset grid 0 150 cross)]
                   [else grid])
                 (cond
                   [(string=? t9 "yes") (underlay/offset grid 150 150 cross)]
                   [else grid])
                 (cond
                   [(not (= 0 a1)) (cond
                                   [(= a1 1) (begin (set! T1 "nein") (set! at1 "yes") (underlay/offset grid -150 -150 circ))]
                                   [(= a1 2) (begin (set! T2 "nein") (set! at2 "yes") (underlay/offset grid 0 -150 circ))]
                                   [(= a1 3) (begin (set! T3 "nein") (set! at3 "yes") (underlay/offset grid 150 -150 circ))]
                                   [(= a1 4) (begin (set! T4 "nein") (set! at4 "yes") (underlay/offset grid -150 0 circ))]
                                   [(= a1 5) (begin (set! T5 "nein") (set! at5 "yes") (underlay/offset grid 0 0 circ))]
                                   [(= a1 6) (begin (set! T6 "nein") (set! at6 "yes") (underlay/offset grid 150 0 circ))]
                                   [(= a1 7) (begin (set! T7 "nein") (set! at7 "yes") (underlay/offset grid -150 150 circ))]
                                   [(= a1 8) (begin (set! T8 "nein") (set! at8 "yes") (underlay/offset grid 0 150 circ))]
                                   [(= a1 9) (begin (set! T9 "nein") (set! at9 "yes") (underlay/offset grid 150 150 circ))])]
                   [(number? ai1) (cond
                                    [(and (not (string=? T1 "nein")) (= ai1 1)) (begin (set! T1 "nein") (set! at1 "yes") (set! a1 ai1) (underlay/offset grid -150 -150 circ))]
                                    [(and (not (string=? T2 "nein")) (= ai1 2)) (begin (set! T2 "nein") (set! at2 "yes") (set! a1 ai1) (underlay/offset grid 0 -150 circ))]
                                    [(and (not (string=? T3 "nein")) (= ai1 3)) (begin (set! T3 "nein") (set! at3 "yes") (set! a1 ai1) (underlay/offset grid 150 -150 circ))]
                                    [(and (not (string=? T4 "nein")) (= ai1 4)) (begin (set! T4 "nein") (set! at4 "yes") (set! a1 ai1) (underlay/offset grid -150 0 circ))]
                                    [(and (not (string=? T5 "nein")) (= ai1 5)) (begin (set! T5 "nein") (set! at5 "yes") (set! a1 ai1) (underlay/offset grid 0 0 circ))]
                                    [(and (not (string=? T6 "nein")) (= ai1 6)) (begin (set! T6 "nein") (set! at6 "yes") (set! a1 ai1) (underlay/offset grid 150 0 circ))]
                                    [(and (not (string=? T7 "nein")) (= ai1 7)) (begin (set! T7 "nein") (set! at7 "yes") (set! a1 ai1) (underlay/offset grid -150 150 circ))]
                                    [(and (not (string=? T8 "nein")) (= ai1 8)) (begin (set! T8 "nein") (set! at8 "yes") (set! a1 ai1) (underlay/offset grid 0 150 circ))]
                                    [(and (not (string=? T9 "nein")) (= ai1 9)) (begin (set! T9 "nein") (set! at9 "yes") (set! a1 ai1) (underlay/offset grid 150 150 circ))]
                                    [else (begin (set! ai1 (random 1 10)) (d x))])]
                   [else grid])
                 (cond
                   [(and (not (= 0 a2)) (not (= ai2 ai1))) (cond
                                                           [(= a2 1) (begin (set! T1 "nein") (set! at1 "yes") (underlay/offset grid -150 -150 circ))]
                                                           [(= a2 2) (begin (set! T2 "nein") (set! at2 "yes") (underlay/offset grid 0 -150 circ))]
                                                           [(= a2 3) (begin (set! T3 "nein") (set! at3 "yes") (underlay/offset grid 150 -150 circ))]
                                                           [(= a2 4) (begin (set! T4 "nein") (set! at4 "yes") (underlay/offset grid -150 0 circ))]
                                                           [(= a2 5) (begin (set! T5 "nein") (set! at5 "yes") (underlay/offset grid 0 0 circ))]
                                                           [(= a2 6) (begin (set! T6 "nein") (set! at6 "yes") (underlay/offset grid 150 0 circ))]
                                                           [(= a2 7) (begin (set! T7 "nein") (set! at7 "yes") (underlay/offset grid -150 150 circ))]
                                                           [(= a2 8) (begin (set! T8 "nein") (set! at8 "yes") (underlay/offset grid 0 150 circ))]
                                                           [(= a2 9) (begin (set! T9 "nein") (set! at9 "yes") (underlay/offset grid 150 150 circ))])]
                   [(number? ai2) (cond
                                    [(= ai2 ai1) (begin (set! ai2 (random 1 10)) (d x))]
                                    [(and (not (string=? T1 "nein")) (= ai2 1)) (begin (set! T1 "nein") (set! at1 "yes") (set! a2 ai2) (underlay/offset grid -150 -150 circ))]
                                    [(and (not (string=? T2 "nein")) (= ai2 2)) (begin (set! T2 "nein") (set! at2 "yes") (set! a2 ai2) (underlay/offset grid 0 -150 circ))]
                                    [(and (not (string=? T3 "nein")) (= ai2 3)) (begin (set! T3 "nein") (set! at3 "yes") (set! a2 ai2) (underlay/offset grid 150 -150 circ))]
                                    [(and (not (string=? T4 "nein")) (= ai2 4)) (begin (set! T4 "nein") (set! at4 "yes") (set! a2 ai2) (underlay/offset grid -150 0 circ))]
                                    [(and (not (string=? T5 "nein")) (= ai2 5)) (begin (set! T5 "nein") (set! at5 "yes") (set! a2 ai2) (underlay/offset grid 0 0 circ))]
                                    [(and (not (string=? T6 "nein")) (= ai2 6)) (begin (set! T6 "nein") (set! at6 "yes") (set! a2 ai2) (underlay/offset grid 150 0 circ))]
                                    [(and (not (string=? T7 "nein")) (= ai2 7)) (begin (set! T7 "nein") (set! at7 "yes") (set! a2 ai2) (underlay/offset grid -150 150 circ))]
                                    [(and (not (string=? T8 "nein")) (= ai2 8)) (begin (set! T8 "nein") (set! at8 "yes") (set! a2 ai2) (underlay/offset grid 0 150 circ))]
                                    [(and (not (string=? T9 "nein")) (= ai2 9)) (begin (set! T9 "nein") (set! at9 "yes") (set! a2 ai2) (underlay/offset grid 150 150 circ))]
                                    [else (begin (set! ai2 (random 1 10)) (d x))])]
                   [else grid])
                 (cond
                   [(and (not (= 0 a3)) (not (or (= ai3 ai2) (= ai3 ai1)))) (cond
                                                                            [(= a3 1) (begin (set! T1 "nein") (set! at1 "yes") (underlay/offset grid -150 -150 circ))]
                                                                            [(= a3 2) (begin (set! T2 "nein") (set! at2 "yes") (underlay/offset grid 0 -150 circ))]
                                                                            [(= a3 3) (begin (set! T3 "nein") (set! at3 "yes") (underlay/offset grid 150 -150 circ))]
                                                                            [(= a3 4) (begin (set! T4 "nein") (set! at4 "yes") (underlay/offset grid -150 0 circ))]
                                                                            [(= a3 5) (begin (set! T5 "nein") (set! at5 "yes") (underlay/offset grid 0 0 circ))]
                                                                            [(= a3 6) (begin (set! T6 "nein") (set! at6 "yes") (underlay/offset grid 150 0 circ))]
                                                                            [(= a3 7) (begin (set! T7 "nein") (set! at7 "yes") (underlay/offset grid -150 150 circ))]
                                                                            [(= a3 8) (begin (set! T8 "nein") (set! at8 "yes") (underlay/offset grid 0 150 circ))]
                                                                            [(= a3 9) (begin (set! T9 "nein") (set! at9 "yes") (underlay/offset grid 150 150 circ))])]
                   [(number? ai3) (cond
                                    [(or (= ai3 ai2) (= ai3 ai1)) (begin (set! ai3 (random 1 10)) (d x))]
                                    [(and (not (string=? T1 "nein")) (= ai3 1)) (begin (set! T1 "nein") (set! at1 "yes") (set! a3 ai3) (underlay/offset grid -150 -150 circ))]
                                    [(and (not (string=? T2 "nein")) (= ai3 2)) (begin (set! T2 "nein") (set! at2 "yes") (set! a3 ai3) (underlay/offset grid 0 -150 circ))]
                                    [(and (not (string=? T3 "nein")) (= ai3 3)) (begin (set! T3 "nein") (set! at3 "yes") (set! a3 ai3) (underlay/offset grid 150 -150 circ))]
                                    [(and (not (string=? T4 "nein")) (= ai3 4)) (begin (set! T4 "nein") (set! at4 "yes") (set! a3 ai3) (underlay/offset grid -150 0 circ))]
                                    [(and (not (string=? T5 "nein")) (= ai3 5)) (begin (set! T5 "nein") (set! at5 "yes") (set! a3 ai3) (underlay/offset grid 0 0 circ))]
                                    [(and (not (string=? T6 "nein")) (= ai3 6)) (begin (set! T6 "nein") (set! at6 "yes") (set! a3 ai3) (underlay/offset grid 150 0 circ))]
                                    [(and (not (string=? T7 "nein")) (= ai3 7)) (begin (set! T7 "nein") (set! at7 "yes") (set! a3 ai3) (underlay/offset grid -150 150 circ))]
                                    [(and (not (string=? T8 "nein")) (= ai3 8)) (begin (set! T8 "nein") (set! at8 "yes") (set! a3 ai3) (underlay/offset grid 0 150 circ))]
                                    [(and (not (string=? T9 "nein")) (= ai3 9)) (begin (set! T9 "nein") (set! at9 "yes") (set! a3 ai3) (underlay/offset grid 150 150 circ))]
                                    [else (begin (set! ai3 (random 1 10)) (d x))])]
                   [else grid])
                 (cond
                   [(and (not (= 0 a4)) (not (or (= ai4 ai2) (= ai4 ai1) (= ai4 ai3)))) (cond
                                                                                       [(= a4 1) (begin (set! T1 "nein") (set! at1 "yes") (underlay/offset grid -150 -150 circ))]
                                                                                       [(= a4 2) (begin (set! T2 "nein") (set! at2 "yes") (underlay/offset grid 0 -150 circ))]
                                                                                       [(= a4 3) (begin (set! T3 "nein") (set! at3 "yes") (underlay/offset grid 150 -150 circ))]
                                                                                       [(= a4 4) (begin (set! T4 "nein") (set! at4 "yes") (underlay/offset grid -150 0 circ))]
                                                                                       [(= a4 5) (begin (set! T5 "nein") (set! at5 "yes") (underlay/offset grid 0 0 circ))]
                                                                                       [(= a4 6) (begin (set! T6 "nein") (set! at6 "yes") (underlay/offset grid 150 0 circ))]
                                                                                       [(= a4 7) (begin (set! T7 "nein") (set! at7 "yes") (underlay/offset grid -150 150 circ))]
                                                                                       [(= a4 8) (begin (set! T8 "nein") (set! at8 "yes") (underlay/offset grid 0 150 circ))]
                                                                                       [(= a4 9) (begin (set! T9 "nein") (set! at9 "yes") (underlay/offset grid 150 150 circ))])]
                   [(number? ai4) (cond
                                    [(or (= ai4 ai2) (= ai4 ai1) (= ai4 ai3)) (begin (set! ai4 (random 1 10)) (d x))]
                                    [(and (not (string=? T1 "nein")) (= ai4 1)) (begin (set! T1 "nein") (set! at1 "yes") (set! a4 ai4) (underlay/offset grid -150 -150 circ))]
                                    [(and (not (string=? T2 "nein")) (= ai4 2)) (begin (set! T2 "nein") (set! at2 "yes") (set! a4 ai4) (underlay/offset grid 0 -150 circ))]
                                    [(and (not (string=? T3 "nein")) (= ai4 3)) (begin (set! T3 "nein") (set! at3 "yes") (set! a4 ai4) (underlay/offset grid 150 -150 circ))]
                                    [(and (not (string=? T4 "nein")) (= ai4 4)) (begin (set! T4 "nein") (set! at4 "yes") (set! a4 ai4) (underlay/offset grid -150 0 circ))]
                                    [(and (not (string=? T5 "nein")) (= ai4 5)) (begin (set! T5 "nein") (set! at5 "yes") (set! a4 ai4) (underlay/offset grid 0 0 circ))]
                                    [(and (not (string=? T6 "nein")) (= ai4 6)) (begin (set! T6 "nein") (set! at6 "yes") (set! a4 ai4) (underlay/offset grid 150 0 circ))]
                                    [(and (not (string=? T7 "nein")) (= ai4 7)) (begin (set! T7 "nein") (set! at7 "yes") (set! a4 ai4) (underlay/offset grid -150 150 circ))]
                                    [(and (not (string=? T8 "nein")) (= ai4 8)) (begin (set! T8 "nein") (set! at8 "yes") (set! a4 ai4) (underlay/offset grid 0 150 circ))]
                                    [(and (not (string=? T9 "nein")) (= ai4 9)) (begin (set! T9 "nein") (set! at9 "yes") (set! a4 ai4) (underlay/offset grid 150 150 circ))]
                                    [else (begin (set! ai4 (random 1 10)) (d x))])]
                   [else grid])))]))


(define (mouse w x y m)
  (cond
    [(string=? m "button-down") (cond
                                  [(and (> y 25) (< y 175) (> x 25) (< x 175) (not (string=? T1 "nein")))
                                   (begin
                                     (set! T1 "nein")
                                     (set! t1 "yes")
                                     (+ w 1))]
                                  [(and (> y 25) (< y 175) (> x 175) (< x 300) (not (string=? T2 "nein")))
                                   (begin
                                     (set! T2 "nein")
                                     (set! t2 "yes")
                                     (+ w 1))]
                                  [(and (> y 25) (< y 175) (> x 325) (< x 450) (not (string=? T3 "nein")))
                                   (begin
                                     (set! T3 "nein")
                                     (set! t3 "yes")
                                     (+ w 1))]
                                  [(and (> y 175) (< y 325) (> x 25) (< x 175) (not (string=? T4 "nein")))
                                   (begin
                                     (set! T4 "nein")
                                     (set! t4 "yes")
                                     (+ w 1))]
                                  [(and (> y 175) (< y 325) (> x 175) (< x 300) (not (string=? T5 "nein")))
                                   (begin
                                     (set! T5 "nein")
                                     (set! t5 "yes")
                                     (+ w 1))]
                                  [(and (> y 175) (< y 325) (> x 325) (< x 450) (not (string=? T6 "nein")))
                                   (begin
                                     (set! T6 "nein")
                                     (set! t6 "yes")
                                     (+ w 1))]
                                  [(and (> y 325) (< y 475) (> x 25) (< x 175) (not (string=? T7 "nein")))
                                   (begin
                                     (set! T7 "nein")
                                     (set! t7 "yes")
                                     (+ w 1))]
                                  [(and (> y 325) (< y 475) (> x 175) (< x 300) (not (string=? T8 "nein")))
                                   (begin
                                     (set! T8 "nein")
                                     (set! t8 "yes")
                                     (+ w 1))]
                                  [(and (> y 325) (< y 475) (> x 325) (< x 450) (not (string=? T9 "nein")))
                                   (begin
                                     (set! T9 "nein")
                                     (set! t9 "yes")
                                     (+ w 1))]
                                  [else w])]
    [else w]))

(big-bang 0
  [to-draw d]
  [on-mouse mouse]
  [name "Tic Tac Toe"])

