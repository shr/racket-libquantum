#lang racket

(require (prefix-in q: "libquantum.rkt"))
(require rackunit
         rackunit/text-ui)

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

(define-test-suite libquantum-tests
  (test-suite
   "Deutsch-Jozsa Algorithm"
   (test-case
    "identity function"
    (check-eq? 1
               (deutsch-jozsa (lambda (x) x))))
   (test-case
    "balanced function"
    (check-eq? 0
               (deutsch-jozsa (lambda (x) (q:cnot 0 2 x)))))))

(run-tests libquantum-tests)
