#lang racket

(require ffi/unsafe)

(define libquantum (ffi-lib "libquantum.so"))

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
   (node _quantum-reg-node-pointer)
   (hash _pointer)))

