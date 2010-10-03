#lang racket

(require ffi/unsafe)

(define libquantum (ffi-lib "libquantum.so"))

(define-syntax defquantum
  (syntax-rules (:)
    [(_ name : type ...)
     (begin
       (define name
         (get-ffi-obj
          (regexp-replaces 'name '((#rx"-" "_")
                                   (#rx"[+*?!]" "")
                                   (#rx"^" "quantum_")))
          libquantum (_fun type ...)))
       (provide name))]))

(define-syntax defgate
  (syntax-rules ()
    [(_ name (arg ...))
     (defquantum name : arg ... -> _void)]))

(define _max-unsigned _uint64)

;;; according to C99
(define-cstruct _complex-float
  ((real _float)
   (imag _float)))

(define-cstruct _quantum-matrix
  ((rows _int)
   (cols _int)
   (t _complex-float-pointer)))

(define-cstruct _quantum-reg-node
  ((amplitude _complex-float)
   (state _uint64)))

(define-cstruct _quantum-reg
  ((width _int)
   (size _int)
   (hashw _int)
   (nodeptr _quantum-reg-node-pointer)
   (hash _pointer)))

(define _quregptr _quantum-reg-pointer)

(define-cstruct _quantum-density-op
  ((num _int)
   (prob _pointer)
   (reg _quregptr)))

(defquantum new-qureg : _max-unsigned _int -> _quantum-reg)
(defquantum new-qureg-size : _int _int -> _quantum-reg)
(defquantum delete-qureg : _quregptr -> _void)
(defquantum print-qureg : _quantum-reg -> _void)
(defquantum addscratch : _int _quregptr -> _void)
(define _function-on-qureg
  (_fun _quregptr -> _void))
(defquantum print-timeop : _int _function-on-qureg -> _void)

; gates
(defgate cnot (_int _int _quregptr))
(defgate toffoli (_int _int _int _quregptr))
; define unbounded toffoli gate here
(defgate sigma-x (_int _quregptr))
(defgate sigma-y (_int _quregptr))
(defgate sigma-z (_int _quregptr))
(defgate gate1 (_int _quantum-matrix _quregptr))
(defgate gate2 (_int _int _quantum-matrix _quregptr))
(defgate r-x (_int _float _quregptr))
(defgate r-y (_int _float _quregptr))
(defgate r-z (_int _float _quregptr))
(defgate phase-scale (_int _float _quregptr))
(defgate phase-kick (_int _float _quregptr))
(defgate hadamard (_int _quregptr))
(defgate walsh (_int _quregptr))
(defgate cond-phase (_int _int _quregptr))
(defgate cond-phase-inv (_int _int _quregptr))
(defgate cond-phase-kick (_int _int _float _quregptr))

(defquantum gate-counter : _int -> _int)

(defgate qft (_int _quregptr))
(defgate qft-inv (_int _quregptr))

(defgate exp-mod-n (_int _int _int _int _quregptr))

(defquantum measure : _quantum-reg -> _max-unsigned)
(defquantum bmeasure : _int _quregptr -> _int)
(defquantum bmeasure-bitpreserve : _int _quregptr -> _int)

(defquantum new-matrix : _int _int -> _quantum-matrix)
(defquantum delete-matrix : _quantum-matrix-pointer -> _void)
(defquantum mmult : _quantum-matrix _quantum-matrix ->
  _quantum-matrix)

(defquantum ipow : _int _int -> _int)
(defquantum gcd : _int _int -> _int)
; (defquantum cancel : (a : (_ptr io _int)) (b : (_ptr io _int)) ->
;  _void -> (list a b))

