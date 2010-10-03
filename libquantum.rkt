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
(defquantum cnot : _int _int _quregptr -> _void)
(defquantum toffoli : _int _int _int _quregptr -> _void)
; define unbounded toffoli gate here
(defquantum sigma-x : _int _quregptr -> _void)
(defquantum sigma-y : _int _quregptr -> _void)
(defquantum sigma-z : _int _quregptr -> _void)
(defquantum gate1 : _int _quantum-matrix _quregptr -> _void)
(defquantum gate2 : _int _int _quantum-matrix _quregptr -> _void)
(defquantum r-x : _int _float _quregptr -> _void)
(defquantum r-y : _int _float _quregptr -> _void)
(defquantum r-z : _int _float _quregptr -> _void)
(defquantum phase-scale : _int _float _quregptr -> _void)
(defquantum phase-kick : _int _float _quregptr -> _void)
(defquantum hadamard : _int _quregptr -> _void)
(defquantum walsh : _int _quregptr -> _void)
(defquantum cond-phase : _int _int _quregptr -> _void)

(define-syntax defgate
  (syntax-rules ()
    [(_ name arg ...)
     (defquantum name : arg ... -> _void)]))

(defgate cond-phase-inv _int _int _quregptr)
(defgate cond-phase-kick _int _int _float _quregptr)

(defquantum gate-counter : _int -> _int)
(defgate qft _int _quregptr)
(defgate qft-inv _int _quregptr)

