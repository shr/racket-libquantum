#lang racket

(require (prefix-in q: "libquantum.rkt"))

; the simple random number generating algorithm

(define (qrng)
  (let ([reg (q:new-qureg 0 1)])
    (q:hadamard 0 reg)
    (q:bmeasure 0 reg)))

