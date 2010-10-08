#lang racket

(require (prefix-in q: "libquantum.rkt"))

; the simple random number generating algorithm

(define (qrng)
  (let ([reg (q:new-qureg 0 1)])
    (q:hadamard 0 reg)
    (q:bmeasure 0 reg)))

; the Deutsch-Jozsa algorithm, from
; http://www.linux-magazin.de/layout/set/print/content/view/full/21495
(define (deutsch-jozsa fun)
  (let ([reg (q:new-qureg 5 3)])
    (for ([i (in-range 3)])
      (q:hadamard i reg))
    (fun reg)
    (for ([i (in-range 2)])
      (q:hadamard i reg))
    (q:bmeasure 0 reg)))
