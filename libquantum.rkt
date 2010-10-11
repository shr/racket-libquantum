#lang racket

(require ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define)
(require ffi/vector)

(define libquantum (ffi-lib "libquantum.so"))

(define-ffi-definer defquant libquantum
  #:provide provide-protected)

;; structure definitions
(define _max-unsigned _uint64)

;;; according to C99
(provide make-complex-float complex-float-real complex-float-imag)
(define-cstruct _complex-float
  ((real _float)
   (imag _float)))

(provide quantum-matrix-rows quantum-matrix-cols quantum-matrix-t)
(define-cstruct _quantum-matrix
  ((rows _int)
   (cols _int)
   (t _complex-float-pointer)))

(provide quantum-reg-node-amplitude quantum-reg-node-state)
(define-cstruct _quantum-reg-node
  ((amplitude _complex-float)
   (state _uint64)))

(provide quantum-reg-width quantum-reg-size quantum-reg-nodeptr
         set-quantum-reg-width!)
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

(defquant delete-qureg (_fun _quantum-reg-pointer -> _void)
  #:c-id quantum_delete_qureg)
(defquant new-qureg (_fun _max-unsigned _int -> _quantum-reg)
  #:c-id quantum_new_qureg
  #:wrap (allocator delete-qureg))
(defquant print-qureg (_fun _quantum-reg -> _void)
  #:c-id quantum_print_qureg)
(defquant add-scratch (_fun _int _quantum-reg-pointer -> _void)
  #:c-id quantum_addscratch)
(defquant kronecker (_fun _quregptr _quregptr -> _quantum-reg)
  #:c-id quantum_kronecker)
(defquant dot-product (_fun _quregptr _quregptr -> _complex-float)
  #:c-id quantum_dot_product)

;; basic operations

(defquant cnot (_fun _int _int _quregptr -> _void)
  #:c-id quantum_cnot)
(defquant toffoli (_fun _int _int _int _quregptr -> _void)
  #:c-id quantum_toffoli)
(defquant sigma-x (_fun _int _quregptr -> _void)
  #:c-id quantum_sigma_x)
(defquant sigma-y (_fun _int _quregptr -> _void)
  #:c-id quantum_sigma_y)
(defquant sigma-z (_fun _int _quregptr -> _void)
  #:c-id quantum_sigma_z)
(defquant rot-x (_fun _int _float _quregptr -> _void)
  #:c-id quantum_r_x)
(defquant rot-y (_fun _int _float _quregptr -> _void)
  #:c-id quantum_r_y)
(defquant rot-z (_fun _int _float _quregptr -> _void)
  #:c-id quantum_r_z)
(defquant phase-scale (_fun _int _float _quregptr -> _void)
  #:c-id quantum_phase_scale)
(defquant phase-kick (_fun _int _float _quregptr -> _void)
  #:c-id quantum_phase_kick)
(defquant hadamard (_fun _int _quregptr -> _void)
  #:c-id quantum_hadamard)
(defquant walsh (_fun _int _quregptr -> _void)
  #:c-id quantum_walsh)
(defquant cond-phase (_fun _int _int _quregptr -> _void)
  #:c-id quantum_cond_phase)
(defquant cond-phase-kick (_fun _int _int _float _quregptr -> _void)
  #:c-id quantum_cond_phase_kick)
(defquant gate1 (_fun _int _quantum-matrix _quregptr -> _void)
  #:c-id quantum_gate1)

(defquant delete-matrix (_fun _quantum-matrix-pointer -> _void)
  #:c-id quantum_delete_matrix)
(defquant new-matrix (_fun _int _int -> _quantum-matrix)
  #:c-id quantum_new_matrix
  #:wrap (allocator delete-matrix))

(defquant gate2 (_fun _int _int _quantum-matrix _quregptr -> _void)
  #:c-id quantum_gate2)

;; algorithms

(defquant exp-mod-n (_fun _int _int _int _int _quregptr -> _void)
  #:c-id quantum_exp_mod_n)
(defquant qft (_fun _int _quregptr -> _void)
  #:c-id quantum_qft)
(defquant qft-inv (_fun _int _quregptr -> _void)
  #:c-id quantum_qft_inv)

;; measurements
(defquant measure (_fun _quantum-reg -> _max-unsigned)
  #:c-id quantum_measure)
(defquant bmeasure (_fun _int _quregptr -> _int)
  #:c-id quantum_bmeasure)
(defquant bmeasure-bitpreserve (_fun _int _quregptr -> _int)
  #:c-id quantum_bmeasure_bitpreserve)

;; decoherence
(defquant set-decoherence! (_fun _float -> _void)
  #:c-id quantum_set_decoherence)
(defquant decohere (_fun _quregptr -> _void)
  #:c-id quantum_decohere)

;; quantum-error-correction

;; the first argument is type: 0 (no QEC), 1 (Steane code)
(defquant qec-encode (_fun _int _int _quregptr -> _void)
  #:c-id quantum_qec_encode)
(defquant qec-decode (_fun _int _int _quregptr -> _void)
  #:c-id quantum_qec_decode)

;; the density operator formalism
(define _qdensptr _quantum-density-op-pointer)
(defquant delete-density-op (_fun _qdensptr -> _void)
  #:c-id quantum_delete_density_op)

(defquant new-density-op (_fun _int
                               _f32vector
                               _quregptr
                               -> _quantum-density-op)
  #:c-id quantum_new_density_op
  #:wrap (allocator delete-density-op))
(defquant qureg->density-op (_fun _quregptr -> _quantum-density-op)
  #:c-id quantum_qureg2density_op)
(defquant print-density-matrix (_fun _qdensptr -> _void)
  #:c-id quantum_print_density_matrix)
(defquant reduce-density-op (_fun _int _qdensptr -> _void)
  #:c-id quantum_reduced_density_op)
(defquant purity (_fun _qdensptr -> _float)
  #:c-id quantum_purity)

;; object code
(defquant objcode-start (_fun -> _void)
  #:c-id quantum_objcode_start)
(defquant objcode-write (_fun _string -> _int)
  #:c-id quantum_objcode_write)
(defquant objcode-stop (_fun -> _void)
  #:c-id quantum_objcode_stop)
(defquant objcode-run (_fun _string _quregptr -> _void)
  #:c-id quantum_objcode_run)

;; time evolution
(define _hamiltonian (_fun _max-unsigned _double -> _quantum-reg))

(defquant rk4 (_fun _quregptr _double _double _hamiltonian _int -> _void)
  #:c-id quantum_rk4)
(defquant rk4-adaptive (_fun _quregptr _double (dt : (_ptr io _double))
                             _hamiltonian -> (step : _double)
                             -> (list dt step))
  #:c-id quantum_rk4a)

;; miscellaneous
(defquant gate-counter (_fun _int -> _int)
  #:c-id quantum_gate_counter)
(defquant prob (_fun _complex-float -> _float)
  #:c-id quantum_prob)
(defquant get-width (_fun _int -> _int)
  #:c-id quantum_getwidth)
(defquant version (_fun -> _string)
  #:c-id quantum_get_version)
(defquant print-timeop (_fun _int (_fun _quregptr -> _void) -> _void)
  #:c-id quantum_print_timeop)

; utility functions
(provide nondestructive-measure ith-node)
(define (nondestructive-measure reg)
  (for/list ([i (in-range (quantum-reg-size reg))])
    (bmeasure-bitpreserve i reg)))

(define (ith-node reg i)
  (unless (< i (quantum-reg-size reg))
    (error 'ith-node (format "size ~a too large for register" i)))
  (ptr-add (quantum-reg-nodeptr reg) i))

