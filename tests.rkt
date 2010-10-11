#lang racket

(require (prefix-in q: "libquantum.rkt"))
(require rackunit
         rackunit/text-ui
         rnrs/arithmetic/bitwise-6)

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

; Grover's algorithm, from the libquantum demos
(define (oracle state reg)
  (define (w) (q:quantum-reg-width reg)) ; reused
  (for ([i (in-range (w))])
    (unless (bitwise-bit-set? state i)
      (q:sigma-x i reg)))
  (q:toffoli 0 1 (+ 1 (w)) reg)
  (for ([i (in-range 1 (w))])
    (q:toffoli i (+ i (w)) (+ i 1 (w)) reg))
  (q:cnot (+ (w) (w)) (w) reg)
  (for ([i (in-range (- (w) 1) 0 -1)])
    (q:toffoli i (+ i (w)) (+ i 1 (w)) reg))
  (q:toffoli 0 1 (+ 1 (w)) reg)
  (for ([i (in-range (w))])
    (unless (bitwise-bit-set? state i)
      (q:sigma-x i reg))))

(define (inversion reg)
  (define (w) (q:quantum-reg-width reg))
  (for ([i (in-range (w))])
    (q:sigma-x i reg))
  (q:hadamard (- (w) 1) reg)
  (if (= 3 (w))
    (q:toffoli 0 1 2 reg)
    (begin
      (q:toffoli 0 1 (+ 1 (w)) reg)
      (for ([i (in-range 1 (- (w) 1))])
        (q:toffoli i (+ i (w)) (+ i 1 (w)) reg))
      (q:cnot (+ (w) (w) -1) (- (w) 1) reg)
      (for ([i (in-range (- (w) 2) 0 -1)])
        (q:toffoli i (+ i (w)) (+ i 1 (w)) reg))
      (q:toffoli 0 1 (+ 1 (w)) reg)))
  (q:hadamard (- (w) 1) reg)
  (for ([i (in-range (w))])
    (q:sigma-x i reg)))

(define (grover target reg)
  (define (w) (q:quantum-reg-width reg))
  (oracle target reg)
  (for ([i (in-range (w))])
    (q:hadamard i reg))
  (inversion reg)
  (for ([i (in-range (w))])
    (q:hadamard i reg)))

(define (grover-main-loop N)
  (let* ([width (q:get-width (+ N 1))]
         [reg (q:new-qureg 0 width)])
    (define (w) (q:quantum-reg-width reg))
    (q:sigma-x (w) reg)
    (for ([i (in-range (w))])
      (q:hadamard i reg))
    (q:hadamard (w) reg)
    (let ([size (* pi 1/4 (sqrt (expt 2 (w))))])
      (printf "iterating ~a times~n" (floor size))
      (for ([i (in-range 1 size)])
        (grover N reg))
      (q:hadamard (w) reg)
      (q:set-quantum-reg-width! reg (+ (w) 1))
      (q:bmeasure (- (w) 1) reg))
    (printf "reg stats: size ~a width ~a~n" (q:quantum-reg-size reg) (w))
    (let ([result #f])
      (for ([i (in-range (q:quantum-reg-size reg))])
        (let* ([n (q:ith-node reg i)]
               [prob (q:prob (q:quantum-reg-node-amplitude n))]
               [state (q:quantum-reg-node-state n)])
          (when (= N state)
            (printf "found ~a with probability ~a~n"
                    state prob)
            (set! result state))))
      result)))

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
               (deutsch-jozsa (lambda (x) (q:cnot 0 2 x))))))
  (test-suite
   "Grover's Algorithm"
   (test-case
    "low value"
    (check-eq? 100
               (grover-main-loop 100)))
   (test-case
    "another value"
    (check-eq? 1000
               (grover-main-loop 1000)))))

(run-tests libquantum-tests)

; this was very useful to debug, along with quobprint
; (q:objcode-start)
; (grover-main-loop 10)
; (q:objcode-write "grover.qcode")
; (q:objcode-stop)
