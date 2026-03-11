;;;; package.lisp
;;;; cl-finite-fields package definition

(defpackage #:cl-finite-fields
  (:use #:cl)
  (:nicknames #:ff)
  (:documentation
   "Finite field arithmetic for cryptographic applications.

Provides operations over prime fields (Fp), extension fields (Fp2, Fp6, Fp12),
Montgomery multiplication, and specialized algorithms for pairing-based
cryptography.

Key Features:
- Prime Field Operations: Addition, subtraction, multiplication, inversion
- Extension Field Tower: Fp -> Fp2 -> Fp6 -> Fp12 (for BLS12-381 pairings)
- Montgomery Multiplication: Efficient modular multiplication
- Field Inversion: Extended GCD and Fermat's little theorem
- Square Roots: Tonelli-Shanks algorithm
- Batch Inversion: Montgomery's trick for O(n) inversions with 1 division
- Constant-Time Operations: Timing-attack resistant primitives

Thread Safety: Yes (all functions are pure)")
  (:export
   ;; Field Constants
   #:+bls12-381-p+
   #:+bls12-381-p-bits+
   #:+bls12-381-p-bytes+
   #:+bls12-381-mont-r+
   #:+bls12-381-mont-r2+
   #:+bls12-381-mont-p-inv+
   #:+secp256k1-p+
   #:+secp256k1-p-bits+
   #:+fp2-non-residue+

   ;; Field Element Types
   #:fp-element
   #:make-fp-element
   #:fp-element-value
   #:fp-element-p
   #:fp-element-modulus

   #:fp2-element
   #:make-fp2-element
   #:fp2-element-c0
   #:fp2-element-c1
   #:fp2-element-p

   #:fp6-element
   #:make-fp6-element
   #:fp6-element-c0
   #:fp6-element-c1
   #:fp6-element-c2
   #:fp6-element-p

   #:fp12-element
   #:make-fp12-element
   #:fp12-element-c0
   #:fp12-element-c1
   #:fp12-element-p

   ;; Montgomery context
   #:montgomery-context
   #:make-montgomery-context
   #:montgomery-context-modulus
   #:montgomery-context-r
   #:montgomery-context-r2
   #:montgomery-context-p-inv
   #:make-default-montgomery-context

   ;; Error conditions
   #:field-error
   #:field-invalid-element-error
   #:field-division-by-zero-error
   #:field-no-square-root-error

   ;; Prime Field Operations (Fp)
   #:fp-add
   #:fp-sub
   #:fp-mul
   #:fp-neg
   #:fp-square
   #:fp-div
   #:fp-pow
   #:fp-pow-vartime
   #:fp-inv
   #:fp-inv-fermat
   #:fp-inv-egcd
   #:fp-reduce

   ;; Square root
   #:fp-sqrt
   #:fp-sqrt-tonelli-shanks
   #:fp-is-square-p
   #:fp-legendre-symbol

   ;; Comparison
   #:fp-equal-p
   #:fp-zero-p
   #:fp-one-p

   ;; Fp2 Operations
   #:fp2-add
   #:fp2-sub
   #:fp2-mul
   #:fp2-square
   #:fp2-neg
   #:fp2-inv
   #:fp2-div
   #:fp2-conjugate
   #:fp2-frobenius
   #:fp2-mul-by-nonresidue
   #:fp2-equal-p
   #:fp2-zero-p
   #:fp2-one-p
   #:fp2-zero
   #:fp2-one

   ;; Fp6 Operations
   #:fp6-add
   #:fp6-sub
   #:fp6-mul
   #:fp6-square
   #:fp6-neg
   #:fp6-inv
   #:fp6-equal-p
   #:fp6-zero-p
   #:fp6-one-p
   #:fp6-zero
   #:fp6-one

   ;; Fp12 Operations
   #:fp12-add
   #:fp12-sub
   #:fp12-mul
   #:fp12-square
   #:fp12-neg
   #:fp12-inv
   #:fp12-conjugate
   #:fp12-cyclotomic-square
   #:fp12-cyclotomic-exp
   #:fp12-equal-p
   #:fp12-zero-p
   #:fp12-one-p
   #:fp12-zero
   #:fp12-one

   ;; Montgomery Multiplication
   #:mont-mul
   #:mont-square
   #:mont-reduce
   #:mont-to-normal
   #:mont-from-normal
   #:mont-pow
   #:mont-inv

   ;; Batch Operations
   #:fp-batch-inv
   #:fp-batch-mul
   #:fp-batch-square
   #:fp-multi-exp

   ;; Constant-Time Operations
   #:ct-fp-add
   #:ct-fp-sub
   #:ct-fp-mul
   #:ct-fp-square
   #:ct-fp-select
   #:ct-fp-cswap
   #:ct-fp-equal-p
   #:ct-fp-is-odd-p

   ;; Serialization
   #:fp-to-bytes
   #:fp-from-bytes
   #:fp-to-hex
   #:fp-from-hex
   #:fp2-to-bytes
   #:fp2-from-bytes
   #:fp6-to-bytes
   #:fp6-from-bytes
   #:fp12-to-bytes
   #:fp12-from-bytes

   ;; Utilities
   #:fp-random
   #:fp2-random
   #:fp-validate
   #:fp2-validate
   #:modular-inverse
   #:extended-gcd
   #:bit-length
   #:count-trailing-zeros
   #:mod-expt
   #:integer-to-bytes
   #:bytes-to-integer))
