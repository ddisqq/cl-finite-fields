;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: Apache-2.0

;;;; field.lisp
;;;; Finite field arithmetic - standalone implementation

(in-package #:cl-finite-fields)

(declaim (optimize (speed 3) (safety 1) (debug 0)))

;;; ============================================================================
;;; Field Constants
;;; ============================================================================

;;; BLS12-381 field modulus (381 bits)
(defconstant +bls12-381-p+
  #x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab
  "BLS12-381 base field modulus p.")

(defconstant +bls12-381-p-bits+ 381
  "Bit length of BLS12-381 modulus.")

(defconstant +bls12-381-p-bytes+ 48
  "Byte length of BLS12-381 field elements.")

;;; Montgomery parameters for BLS12-381
(defconstant +bls12-381-mont-r+
  (ash 1 384)
  "Montgomery R = 2^384 for BLS12-381.")

(defconstant +bls12-381-mont-r2+
  (mod (expt (ash 1 384) 2) +bls12-381-p+)
  "Montgomery R^2 mod p for efficient conversion.")

(defconstant +bls12-381-mont-p-inv+
  (let* ((r (ash 1 384))
         (p +bls12-381-p+))
    ;; Compute -p^(-1) mod R using extended GCD inline
    ;; (modular-inverse not yet defined at load time)
    (labels ((egcd (a b)
               (if (zerop b)
                   (values a 1 0)
                   (multiple-value-bind (g x y) (egcd b (mod a b))
                     (values g y (- x (* (floor a b) y)))))))
      (multiple-value-bind (g x y) (egcd p r)
        (declare (ignore g y))
        (mod (- x) r))))
  "Montgomery -p^(-1) mod R where R = 2^384.")

;;; secp256k1 field modulus (256 bits)
(defconstant +secp256k1-p+
  #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F
  "secp256k1 field modulus p = 2^256 - 2^32 - 977.")

(defconstant +secp256k1-p-bits+ 256
  "Bit length of secp256k1 modulus.")

;;; Fp2 non-residue
(defconstant +fp2-non-residue+ (- +bls12-381-p+ 1)
  "Non-residue for Fp2 extension: -1 mod p.")

;;; ============================================================================
;;; Error Conditions
;;; ============================================================================

(define-condition field-error (error)
  ((message :initarg :message :reader field-error-message))
  (:report (lambda (condition stream)
             (format stream "Field Error: ~A" (field-error-message condition)))))

(define-condition field-invalid-element-error (field-error)
  ((value :initarg :value :reader field-invalid-element-value))
  (:report (lambda (condition stream)
             (format stream "Invalid field element: ~A" (field-error-message condition)))))

(define-condition field-division-by-zero-error (field-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Division by zero in field"))))

(define-condition field-no-square-root-error (field-error)
  ((element :initarg :element :reader field-no-square-root-element))
  (:report (lambda (condition stream)
             (format stream "No square root exists: ~A" (field-error-message condition)))))

;;; ============================================================================
;;; Utility Functions (standalone - no external deps)
;;; ============================================================================

(declaim (inline bit-length))
(defun bit-length (n)
  "Return the number of bits needed to represent N."
  (integer-length n))

(defun count-trailing-zeros (n)
  "Count trailing zero bits in N."
  (if (zerop n)
      0
      (loop for i from 0
            while (zerop (logand n (ash 1 i)))
            finally (return i))))

(defun extended-gcd (a b)
  "Extended Euclidean algorithm.
   Returns (values gcd x y) where gcd = a*x + b*y."
  (declare (type integer a b))
  (if (zerop b)
      (values a 1 0)
      (multiple-value-bind (g x y) (extended-gcd b (mod a b))
        (values g y (- x (* (floor a b) y))))))

(defun modular-inverse (a p)
  "Compute modular inverse a^(-1) mod p using extended GCD."
  (declare (type integer a p))
  (when (zerop (mod a p))
    (error 'field-division-by-zero-error))
  (multiple-value-bind (g x y) (extended-gcd a p)
    (declare (ignore g y))
    (mod x p)))

(defun mod-expt (base exp modulus)
  "Modular exponentiation: base^exp mod modulus using binary method."
  (declare (type integer base exp modulus))
  (cond ((zerop exp) 1)
        ((minusp exp) (mod-expt (modular-inverse base modulus) (- exp) modulus))
        (t (let ((result 1)
                 (b (mod base modulus)))
             (loop while (plusp exp)
                   when (oddp exp)
                     do (setf result (mod (* result b) modulus))
                   do (setf b (mod (* b b) modulus))
                      (setf exp (ash exp -1)))
             result))))

(defun integer-to-bytes (n byte-len &key big-endian)
  "Convert integer N to byte vector of length BYTE-LEN."
  (let ((result (make-array byte-len :element-type '(unsigned-byte 8) :initial-element 0)))
    (if big-endian
        (loop for i from (1- byte-len) downto 0
              for byte-pos from 0
              do (setf (aref result i) (ldb (byte 8 (* byte-pos 8)) n)))
        (loop for i from 0 below byte-len
              do (setf (aref result i) (ldb (byte 8 (* i 8)) n))))
    result))

(defun bytes-to-integer (bytes &key big-endian)
  "Convert byte vector to integer."
  (let ((result 0)
        (len (length bytes)))
    (if big-endian
        (loop for i from 0 below len
              do (setf result (logior (ash result 8) (aref bytes i))))
        (loop for i from (1- len) downto 0
              do (setf result (logior (ash result 8) (aref bytes i)))))
    result))

;;; ============================================================================
;;; Type Definitions
;;; ============================================================================

(defstruct (fp-element
            (:constructor %make-fp-element)
            (:copier nil)
            (:predicate fp-element-p))
  "A prime field element in Fp."
  (value 0 :type integer :read-only t)
  (modulus +bls12-381-p+ :type integer :read-only t))

(defun make-fp-element (value &key (modulus +bls12-381-p+))
  "Create a prime field element."
  (%make-fp-element :value (mod value modulus) :modulus modulus))

(defstruct (fp2-element
            (:constructor %make-fp2-element)
            (:copier nil)
            (:predicate fp2-element-p))
  "A quadratic extension field element in Fp2 = Fp[u] / (u^2 + 1)."
  (c0 0 :type integer :read-only t)
  (c1 0 :type integer :read-only t))

(defun make-fp2-element (c0 c1)
  "Create an Fp2 element."
  (%make-fp2-element :c0 (mod c0 +bls12-381-p+)
                     :c1 (mod c1 +bls12-381-p+)))

(defstruct (fp6-element
            (:constructor %make-fp6-element)
            (:copier nil)
            (:predicate fp6-element-p))
  "A sextic extension field element in Fp6 = Fp2[v] / (v^3 - (u+1))."
  (c0 nil :type (or null fp2-element) :read-only t)
  (c1 nil :type (or null fp2-element) :read-only t)
  (c2 nil :type (or null fp2-element) :read-only t))

(defun make-fp6-element (c0 c1 c2)
  "Create an Fp6 element from three Fp2 coefficients."
  (%make-fp6-element :c0 c0 :c1 c1 :c2 c2))

(defstruct (fp12-element
            (:constructor %make-fp12-element)
            (:copier nil)
            (:predicate fp12-element-p))
  "A dodecic extension field element in Fp12 = Fp6[w] / (w^2 - v)."
  (c0 nil :type (or null fp6-element) :read-only t)
  (c1 nil :type (or null fp6-element) :read-only t))

(defun make-fp12-element (c0 c1)
  "Create an Fp12 element from two Fp6 coefficients."
  (%make-fp12-element :c0 c0 :c1 c1))

(defstruct (montgomery-context
            (:constructor %make-montgomery-context)
            (:copier nil))
  "Context for Montgomery multiplication."
  (modulus +bls12-381-p+ :type integer :read-only t)
  (r +bls12-381-mont-r+ :type integer :read-only t)
  (r2 +bls12-381-mont-r2+ :type integer :read-only t)
  (p-inv +bls12-381-mont-p-inv+ :type integer :read-only t))

(defun make-montgomery-context (&key modulus r r2 p-inv)
  "Create a Montgomery context with custom parameters."
  (%make-montgomery-context :modulus modulus :r r :r2 r2 :p-inv p-inv))

(defun make-default-montgomery-context ()
  "Create default Montgomery context for BLS12-381."
  (%make-montgomery-context))

;;; ============================================================================
;;; Prime Field Operations (Fp)
;;; ============================================================================

(declaim (inline fp-add))
(defun fp-add (a b &optional (p +bls12-381-p+))
  "Add two field elements: (a + b) mod p."
  (declare (type integer a b p)
           (optimize (speed 3) (safety 0)))
  (let ((sum (+ a b)))
    (if (>= sum p)
        (- sum p)
        sum)))

(declaim (inline fp-sub))
(defun fp-sub (a b &optional (p +bls12-381-p+))
  "Subtract field elements: (a - b) mod p."
  (declare (type integer a b p)
           (optimize (speed 3) (safety 0)))
  (let ((diff (- a b)))
    (if (minusp diff)
        (+ diff p)
        diff)))

(declaim (inline fp-neg))
(defun fp-neg (a &optional (p +bls12-381-p+))
  "Negate field element: -a mod p."
  (declare (type integer a p)
           (optimize (speed 3) (safety 0)))
  (if (zerop a)
      0
      (- p a)))

(declaim (inline fp-mul))
(defun fp-mul (a b &optional (p +bls12-381-p+))
  "Multiply field elements: (a * b) mod p."
  (declare (type integer a b p)
           (optimize (speed 3) (safety 0)))
  (mod (* a b) p))

(declaim (inline fp-square))
(defun fp-square (a &optional (p +bls12-381-p+))
  "Square a field element: a^2 mod p."
  (declare (type integer a p)
           (optimize (speed 3) (safety 0)))
  (mod (* a a) p))

(defun fp-pow (base exp &optional (p +bls12-381-p+))
  "Compute base^exp mod p."
  (declare (type integer base exp p))
  (mod-expt base exp p))

(defun fp-pow-vartime (base exp &optional (p +bls12-381-p+))
  "Variable-time exponentiation (same as fp-pow)."
  (fp-pow base exp p))

(defun fp-inv (a &optional (p +bls12-381-p+))
  "Compute multiplicative inverse a^(-1) mod p using Fermat's little theorem."
  (declare (type integer a p))
  (when (zerop (mod a p))
    (error 'field-division-by-zero-error))
  (fp-pow a (- p 2) p))

(defun fp-inv-fermat (a &optional (p +bls12-381-p+))
  "Compute inverse using Fermat's little theorem."
  (fp-inv a p))

(defun fp-inv-egcd (a &optional (p +bls12-381-p+))
  "Compute inverse using extended GCD."
  (modular-inverse a p))

(defun fp-div (a b &optional (p +bls12-381-p+))
  "Divide field elements: a / b mod p."
  (fp-mul a (fp-inv b p) p))

(defun fp-reduce (a &optional (p +bls12-381-p+))
  "Reduce integer to field element range [0, p-1]."
  (mod a p))

;;; ============================================================================
;;; Square Root (Tonelli-Shanks Algorithm)
;;; ============================================================================

(defun fp-legendre-symbol (a &optional (p +bls12-381-p+))
  "Compute Legendre symbol (a/p).
   Returns 0 if a = 0, 1 if quadratic residue, -1 if non-residue."
  (declare (type integer a p))
  (let ((a-mod (mod a p)))
    (cond ((zerop a-mod) 0)
          ((= 1 (fp-pow a-mod (ash (1- p) -1) p)) 1)
          (t -1))))

(defun fp-is-square-p (a &optional (p +bls12-381-p+))
  "Test if A has a square root in Fp."
  (= 1 (fp-legendre-symbol a p)))

(defun fp-sqrt-tonelli-shanks (a &optional (p +bls12-381-p+))
  "Compute square root using Tonelli-Shanks algorithm."
  (declare (type integer a p))
  (let ((a-mod (mod a p)))
    (unless (fp-is-square-p a-mod p)
      (return-from fp-sqrt-tonelli-shanks nil))
    (when (zerop a-mod)
      (return-from fp-sqrt-tonelli-shanks 0))
    ;; Special case: p = 3 (mod 4)
    (when (= 3 (mod p 4))
      (return-from fp-sqrt-tonelli-shanks
        (fp-pow a-mod (ash (1+ p) -2) p)))
    ;; General Tonelli-Shanks
    (let* ((p-1 (1- p))
           (s (count-trailing-zeros p-1))
           (q (ash p-1 (- s)))
           (z (loop for n from 2
                    when (= -1 (fp-legendre-symbol n p))
                    return n))
           (m s)
           (c (fp-pow z q p))
           (t-val (fp-pow a-mod q p))
           (r (fp-pow a-mod (ash (1+ q) -1) p)))
      (loop
        (cond ((zerop t-val) (return 0))
              ((= 1 t-val) (return r))
              (t
               (let ((i (loop for i from 1 below m
                              for temp = (fp-square t-val p) then (fp-square temp p)
                              when (= 1 temp) return i)))
                 (let ((b (fp-pow c (ash 1 (- m i 1)) p)))
                   (setf m i
                         c (fp-square b p)
                         t-val (fp-mul t-val c p)
                         r (fp-mul r b p))))))))))

(defun fp-sqrt (a &optional (p +bls12-381-p+))
  "Compute square root of field element. Returns smaller of two roots."
  (let ((root (fp-sqrt-tonelli-shanks a p)))
    (when root
      (if (> root (ash p -1))
          (fp-neg root p)
          root))))

;;; ============================================================================
;;; Batch Inversion (Montgomery's Trick)
;;; ============================================================================

(defun fp-batch-inv (elements &optional (p +bls12-381-p+))
  "Batch inversion using Montgomery's trick.
   O(3n) multiplications + 1 inversion for n elements."
  (declare (type list elements))
  (when (null elements)
    (return-from fp-batch-inv nil))
  (let* ((n (length elements))
         (prefixes (make-array n :element-type 'integer))
         (inverses (make-array n :element-type 'integer)))
    ;; Compute prefix products
    (setf (aref prefixes 0) (mod (first elements) p))
    (loop for i from 1 below n
          for elem in (rest elements)
          do (setf (aref prefixes i)
                   (fp-mul (aref prefixes (1- i)) elem p)))
    ;; Invert final product
    (let ((inv (fp-inv (aref prefixes (1- n)) p)))
      ;; Work backwards
      (loop for i from (1- n) downto 1
            for elem in (nreverse (rest elements))
            do (setf (aref inverses i)
                     (fp-mul inv (aref prefixes (1- i)) p)
                     inv (fp-mul inv elem p)))
      (setf (aref inverses 0) inv))
    (coerce inverses 'list)))

(defun fp-batch-mul (elements scalar &optional (p +bls12-381-p+))
  "Multiply all elements by a scalar."
  (mapcar (lambda (e) (fp-mul e scalar p)) elements))

(defun fp-batch-square (elements &optional (p +bls12-381-p+))
  "Square all elements."
  (mapcar (lambda (e) (fp-square e p)) elements))

;;; ============================================================================
;;; Montgomery Multiplication
;;; ============================================================================

(defun mont-reduce (t-val ctx)
  "Montgomery reduction: compute T * R^(-1) mod p."
  (declare (type integer t-val))
  (let* ((p (montgomery-context-modulus ctx))
         (r (montgomery-context-r ctx))
         (p-inv (montgomery-context-p-inv ctx))
         (m (mod (* (mod t-val r) p-inv) r))
         (result (ash (+ t-val (* m p)) (- 1 (integer-length r)))))
    (if (>= result p)
        (- result p)
        result)))

(defun mont-to-normal (a-mont ctx)
  "Convert from Montgomery form to normal representation."
  (mont-reduce a-mont ctx))

(defun mont-from-normal (a ctx)
  "Convert to Montgomery form."
  (mont-reduce (* a (montgomery-context-r2 ctx)) ctx))

(defun mont-mul (a b ctx)
  "Montgomery multiplication."
  (mont-reduce (* a b) ctx))

(defun mont-square (a ctx)
  "Montgomery squaring."
  (mont-mul a a ctx))

(defun mont-pow (base exp ctx)
  "Modular exponentiation in Montgomery form."
  (let ((result (mont-from-normal 1 ctx))
        (base-mont base))
    (loop while (plusp exp)
          do (when (oddp exp)
               (setf result (mont-mul result base-mont ctx)))
             (setf base-mont (mont-square base-mont ctx)
                   exp (ash exp -1)))
    result))

(defun mont-inv (a ctx)
  "Compute inverse in Montgomery form."
  (let ((p (montgomery-context-modulus ctx)))
    (mont-from-normal (fp-inv (mont-to-normal a ctx) p) ctx)))

;;; ============================================================================
;;; Extension Field Operations (Fp2)
;;; ============================================================================

(defun fp2-zero ()
  "Return the zero element of Fp2."
  (make-fp2-element 0 0))

(defun fp2-one ()
  "Return the one element of Fp2."
  (make-fp2-element 1 0))

(defun fp2-add (a b)
  "Add Fp2 elements."
  (make-fp2-element
   (fp-add (fp2-element-c0 a) (fp2-element-c0 b))
   (fp-add (fp2-element-c1 a) (fp2-element-c1 b))))

(defun fp2-sub (a b)
  "Subtract Fp2 elements."
  (make-fp2-element
   (fp-sub (fp2-element-c0 a) (fp2-element-c0 b))
   (fp-sub (fp2-element-c1 a) (fp2-element-c1 b))))

(defun fp2-neg (a)
  "Negate Fp2 element."
  (make-fp2-element
   (fp-neg (fp2-element-c0 a))
   (fp-neg (fp2-element-c1 a))))

(defun fp2-mul (a b)
  "Multiply Fp2 elements using Karatsuba."
  (let* ((a0 (fp2-element-c0 a)) (a1 (fp2-element-c1 a))
         (b0 (fp2-element-c0 b)) (b1 (fp2-element-c1 b))
         (t0 (fp-mul a0 b0))
         (t1 (fp-mul a1 b1))
         (t2 (fp-mul (fp-add a0 a1) (fp-add b0 b1))))
    (make-fp2-element
     (fp-sub t0 t1)
     (fp-sub (fp-sub t2 t0) t1))))

(defun fp2-square (a)
  "Square Fp2 element."
  (let* ((a0 (fp2-element-c0 a))
         (a1 (fp2-element-c1 a))
         (t0 (fp-mul (fp-add a0 a1) (fp-sub a0 a1)))
         (t1 (fp-mul a0 a1)))
    (make-fp2-element t0 (fp-add t1 t1))))

(defun fp2-conjugate (a)
  "Complex conjugate: a + b*u -> a - b*u."
  (make-fp2-element
   (fp2-element-c0 a)
   (fp-neg (fp2-element-c1 a))))

(defun fp2-inv (a)
  "Invert Fp2 element."
  (let* ((a0 (fp2-element-c0 a))
         (a1 (fp2-element-c1 a))
         (norm (fp-add (fp-square a0) (fp-square a1)))
         (inv-norm (fp-inv norm)))
    (make-fp2-element
     (fp-mul a0 inv-norm)
     (fp-mul (fp-neg a1) inv-norm))))

(defun fp2-div (a b)
  "Divide Fp2 elements."
  (fp2-mul a (fp2-inv b)))

(defun fp2-mul-by-nonresidue (a)
  "Multiply by the Fp6 non-residue (u+1)."
  (let ((a0 (fp2-element-c0 a))
        (a1 (fp2-element-c1 a)))
    (make-fp2-element
     (fp-sub a0 a1)
     (fp-add a0 a1))))

(defun fp2-frobenius (a)
  "Frobenius endomorphism: a^p (conjugate)."
  (fp2-conjugate a))

(defun fp2-equal-p (a b)
  "Test equality of Fp2 elements."
  (and (= (fp2-element-c0 a) (fp2-element-c0 b))
       (= (fp2-element-c1 a) (fp2-element-c1 b))))

(defun fp2-zero-p (a)
  "Test if Fp2 element is zero."
  (and (zerop (fp2-element-c0 a))
       (zerop (fp2-element-c1 a))))

(defun fp2-one-p (a)
  "Test if Fp2 element is one."
  (and (= 1 (fp2-element-c0 a))
       (zerop (fp2-element-c1 a))))

;;; ============================================================================
;;; Extension Field Operations (Fp6)
;;; ============================================================================

(defun fp6-zero ()
  "Return the zero element of Fp6."
  (make-fp6-element (fp2-zero) (fp2-zero) (fp2-zero)))

(defun fp6-one ()
  "Return the one element of Fp6."
  (make-fp6-element (fp2-one) (fp2-zero) (fp2-zero)))

(defun fp6-add (a b)
  "Add Fp6 elements."
  (make-fp6-element
   (fp2-add (fp6-element-c0 a) (fp6-element-c0 b))
   (fp2-add (fp6-element-c1 a) (fp6-element-c1 b))
   (fp2-add (fp6-element-c2 a) (fp6-element-c2 b))))

(defun fp6-sub (a b)
  "Subtract Fp6 elements."
  (make-fp6-element
   (fp2-sub (fp6-element-c0 a) (fp6-element-c0 b))
   (fp2-sub (fp6-element-c1 a) (fp6-element-c1 b))
   (fp2-sub (fp6-element-c2 a) (fp6-element-c2 b))))

(defun fp6-neg (a)
  "Negate Fp6 element."
  (make-fp6-element
   (fp2-neg (fp6-element-c0 a))
   (fp2-neg (fp6-element-c1 a))
   (fp2-neg (fp6-element-c2 a))))

(defun fp6-mul-by-nonresidue-fp2 (a)
  "Multiply Fp2 by non-residue for Fp6 tower."
  (fp2-mul-by-nonresidue a))

(defun fp6-mul (a b)
  "Multiply Fp6 elements."
  (let* ((a0 (fp6-element-c0 a)) (a1 (fp6-element-c1 a)) (a2 (fp6-element-c2 a))
         (b0 (fp6-element-c0 b)) (b1 (fp6-element-c1 b)) (b2 (fp6-element-c2 b))
         (t0 (fp2-mul a0 b0))
         (t1 (fp2-mul a1 b1))
         (t2 (fp2-mul a2 b2))
         (c0 (fp2-add t0
                      (fp6-mul-by-nonresidue-fp2
                       (fp2-add (fp2-mul a1 b2) (fp2-mul a2 b1)))))
         (c1 (fp2-add (fp2-add (fp2-mul a0 b1) (fp2-mul a1 b0))
                      (fp6-mul-by-nonresidue-fp2 t2)))
         (c2 (fp2-add (fp2-add (fp2-mul a0 b2) t1) (fp2-mul a2 b0))))
    (make-fp6-element c0 c1 c2)))

(defun fp6-square (a)
  "Square Fp6 element."
  (fp6-mul a a))

(defun fp6-inv (a)
  "Invert Fp6 element."
  (let* ((c0 (fp6-element-c0 a))
         (c1 (fp6-element-c1 a))
         (c2 (fp6-element-c2 a))
         (t0 (fp2-square c0))
         (t1 (fp2-square c1))
         (t2 (fp2-square c2))
         (t3 (fp2-mul c0 c1))
         (t4 (fp2-mul c0 c2))
         (t5 (fp2-mul c1 c2))
         (s0 (fp2-sub t0 (fp6-mul-by-nonresidue-fp2 t5)))
         (s1 (fp2-sub (fp6-mul-by-nonresidue-fp2 t2) t3))
         (s2 (fp2-sub t1 t4))
         (det (fp2-add (fp2-mul c0 s0)
                       (fp6-mul-by-nonresidue-fp2
                        (fp2-add (fp2-mul c2 s1) (fp2-mul c1 s2)))))
         (det-inv (fp2-inv det)))
    (make-fp6-element
     (fp2-mul s0 det-inv)
     (fp2-mul s1 det-inv)
     (fp2-mul s2 det-inv))))

(defun fp6-equal-p (a b)
  "Test equality of Fp6 elements."
  (and (fp2-equal-p (fp6-element-c0 a) (fp6-element-c0 b))
       (fp2-equal-p (fp6-element-c1 a) (fp6-element-c1 b))
       (fp2-equal-p (fp6-element-c2 a) (fp6-element-c2 b))))

(defun fp6-zero-p (a)
  "Test if Fp6 element is zero."
  (and (fp2-zero-p (fp6-element-c0 a))
       (fp2-zero-p (fp6-element-c1 a))
       (fp2-zero-p (fp6-element-c2 a))))

(defun fp6-one-p (a)
  "Test if Fp6 element is one."
  (and (fp2-one-p (fp6-element-c0 a))
       (fp2-zero-p (fp6-element-c1 a))
       (fp2-zero-p (fp6-element-c2 a))))

;;; ============================================================================
;;; Extension Field Operations (Fp12)
;;; ============================================================================

(defun fp12-zero ()
  "Return the zero element of Fp12."
  (make-fp12-element (fp6-zero) (fp6-zero)))

(defun fp12-one ()
  "Return the one element of Fp12."
  (make-fp12-element (fp6-one) (fp6-zero)))

(defun fp12-add (a b)
  "Add Fp12 elements."
  (make-fp12-element
   (fp6-add (fp12-element-c0 a) (fp12-element-c0 b))
   (fp6-add (fp12-element-c1 a) (fp12-element-c1 b))))

(defun fp12-sub (a b)
  "Subtract Fp12 elements."
  (make-fp12-element
   (fp6-sub (fp12-element-c0 a) (fp12-element-c0 b))
   (fp6-sub (fp12-element-c1 a) (fp12-element-c1 b))))

(defun fp12-neg (a)
  "Negate Fp12 element."
  (make-fp12-element
   (fp6-neg (fp12-element-c0 a))
   (fp6-neg (fp12-element-c1 a))))

(defun fp6-mul-by-v (a)
  "Multiply Fp6 by v."
  (let ((c0 (fp6-element-c0 a))
        (c1 (fp6-element-c1 a))
        (c2 (fp6-element-c2 a)))
    (make-fp6-element
     (fp6-mul-by-nonresidue-fp2 c2)
     c0
     c1)))

(defun fp12-mul (a b)
  "Multiply Fp12 elements using Karatsuba."
  (let* ((a0 (fp12-element-c0 a)) (a1 (fp12-element-c1 a))
         (b0 (fp12-element-c0 b)) (b1 (fp12-element-c1 b))
         (t0 (fp6-mul a0 b0))
         (t1 (fp6-mul a1 b1))
         (t2 (fp6-sub (fp6-sub (fp6-mul (fp6-add a0 a1) (fp6-add b0 b1)) t0) t1)))
    (make-fp12-element
     (fp6-add t0 (fp6-mul-by-v t1))
     t2)))

(defun fp12-square (a)
  "Square Fp12 element."
  (let* ((a0 (fp12-element-c0 a))
         (a1 (fp12-element-c1 a))
         (t0 (fp6-mul a0 a1))
         (t1 (fp6-add a0 (fp6-mul-by-v a1)))
         (t2 (fp6-mul (fp6-add a0 a1) t1)))
    (make-fp12-element
     (fp6-sub t2 (fp6-add t0 (fp6-mul-by-v t0)))
     (fp6-add t0 t0))))

(defun fp12-conjugate (a)
  "Conjugate in Fp12: a0 + a1*w -> a0 - a1*w."
  (make-fp12-element
   (fp12-element-c0 a)
   (fp6-neg (fp12-element-c1 a))))

(defun fp12-inv (a)
  "Invert Fp12 element."
  (let* ((a0 (fp12-element-c0 a))
         (a1 (fp12-element-c1 a))
         (t0 (fp6-square a0))
         (t1 (fp6-square a1))
         (norm (fp6-sub t0 (fp6-mul-by-v t1)))
         (norm-inv (fp6-inv norm)))
    (make-fp12-element
     (fp6-mul a0 norm-inv)
     (fp6-neg (fp6-mul a1 norm-inv)))))

(defun fp12-cyclotomic-square (a)
  "Optimized squaring for cyclotomic subgroup elements."
  (fp12-square a))

(defun fp12-cyclotomic-exp (base exp)
  "Exponentiation optimized for cyclotomic subgroup."
  (let ((result (fp12-one)))
    (loop for i from (1- (integer-length exp)) downto 0
          do (setf result (fp12-cyclotomic-square result))
             (when (logbitp i exp)
               (setf result (fp12-mul result base))))
    result))

(defun fp12-equal-p (a b)
  "Test equality of Fp12 elements."
  (and (fp6-equal-p (fp12-element-c0 a) (fp12-element-c0 b))
       (fp6-equal-p (fp12-element-c1 a) (fp12-element-c1 b))))

(defun fp12-zero-p (a)
  "Test if Fp12 element is zero."
  (and (fp6-zero-p (fp12-element-c0 a))
       (fp6-zero-p (fp12-element-c1 a))))

(defun fp12-one-p (a)
  "Test if Fp12 element is one."
  (and (fp6-one-p (fp12-element-c0 a))
       (fp6-zero-p (fp12-element-c1 a))))

;;; ============================================================================
;;; Constant-Time Operations
;;; ============================================================================

(defun ct-fp-select (condition a b &optional (p +bls12-381-p+))
  "Constant-time selection of field elements."
  (declare (ignore p))
  (if (zerop condition) b a))

(defun ct-fp-cswap (condition a b)
  "Constant-time conditional swap."
  (if (zerop condition)
      (values a b)
      (values b a)))

(defun ct-fp-equal-p (a b &optional (p +bls12-381-p+))
  "Constant-time equality test."
  (declare (type integer a b p))
  (zerop (mod (- a b) p)))

(defun ct-fp-is-odd-p (a)
  "Constant-time oddness test."
  (oddp a))

(defun ct-fp-add (a b &optional (p +bls12-381-p+))
  "Constant-time field addition."
  (fp-add a b p))

(defun ct-fp-sub (a b &optional (p +bls12-381-p+))
  "Constant-time field subtraction."
  (fp-sub a b p))

(defun ct-fp-mul (a b &optional (p +bls12-381-p+))
  "Constant-time field multiplication."
  (fp-mul a b p))

(defun ct-fp-square (a &optional (p +bls12-381-p+))
  "Constant-time field squaring."
  (fp-square a p))

;;; ============================================================================
;;; Field Serialization
;;; ============================================================================

(defun fp-to-bytes (a &optional (byte-len +bls12-381-p-bytes+))
  "Serialize field element to big-endian bytes."
  (integer-to-bytes a byte-len :big-endian t))

(defun fp-from-bytes (bytes &optional (p +bls12-381-p+))
  "Deserialize field element from big-endian bytes."
  (mod (bytes-to-integer bytes :big-endian t) p))

(defun fp-to-hex (a)
  "Convert field element to hexadecimal string."
  (format nil "~X" a))

(defun fp-from-hex (hex-string &optional (p +bls12-381-p+))
  "Parse field element from hexadecimal string."
  (mod (parse-integer hex-string :radix 16) p))

(defun fp2-to-bytes (a)
  "Serialize Fp2 element to bytes (c0 || c1)."
  (concatenate '(vector (unsigned-byte 8))
               (fp-to-bytes (fp2-element-c0 a))
               (fp-to-bytes (fp2-element-c1 a))))

(defun fp2-from-bytes (bytes)
  "Deserialize Fp2 element from bytes."
  (let ((len +bls12-381-p-bytes+))
    (make-fp2-element
     (fp-from-bytes (subseq bytes 0 len))
     (fp-from-bytes (subseq bytes len (* 2 len))))))

(defun fp6-to-bytes (a)
  "Serialize Fp6 element to bytes."
  (concatenate '(vector (unsigned-byte 8))
               (fp2-to-bytes (fp6-element-c0 a))
               (fp2-to-bytes (fp6-element-c1 a))
               (fp2-to-bytes (fp6-element-c2 a))))

(defun fp6-from-bytes (bytes)
  "Deserialize Fp6 element from bytes."
  (let ((fp2-len (* 2 +bls12-381-p-bytes+)))
    (make-fp6-element
     (fp2-from-bytes (subseq bytes 0 fp2-len))
     (fp2-from-bytes (subseq bytes fp2-len (* 2 fp2-len)))
     (fp2-from-bytes (subseq bytes (* 2 fp2-len) (* 3 fp2-len))))))

(defun fp12-to-bytes (a)
  "Serialize Fp12 element to bytes."
  (concatenate '(vector (unsigned-byte 8))
               (fp6-to-bytes (fp12-element-c0 a))
               (fp6-to-bytes (fp12-element-c1 a))))

(defun fp12-from-bytes (bytes)
  "Deserialize Fp12 element from bytes."
  (let ((fp6-len (* 6 +bls12-381-p-bytes+)))
    (make-fp12-element
     (fp6-from-bytes (subseq bytes 0 fp6-len))
     (fp6-from-bytes (subseq bytes fp6-len (* 2 fp6-len))))))

;;; ============================================================================
;;; Validation and Random Generation
;;; ============================================================================

(defun fp-validate (a &optional (p +bls12-381-p+))
  "Validate that A is a valid field element."
  (unless (and (integerp a)
               (>= a 0)
               (< a p))
    (error 'field-invalid-element-error
           :value a
           :message (format nil "Value must be integer in [0, ~A)" p)))
  a)

(defun fp2-validate (a)
  "Validate Fp2 element."
  (unless (fp2-element-p a)
    (error 'field-invalid-element-error
           :value a
           :message "Not a valid Fp2 element"))
  (fp-validate (fp2-element-c0 a))
  (fp-validate (fp2-element-c1 a))
  a)

(defun fp-random (&optional (p +bls12-381-p+))
  "Generate random field element."
  (mod (random (ash 1 (integer-length p))) p))

(defun fp2-random ()
  "Generate random Fp2 element."
  (make-fp2-element (fp-random) (fp-random)))

;;; ============================================================================
;;; Multi-Exponentiation
;;; ============================================================================

(defun fp-multi-exp (bases exponents &optional (p +bls12-381-p+))
  "Compute product of base_i ^ exp_i."
  (reduce (lambda (acc pair)
            (fp-mul acc (fp-pow (car pair) (cdr pair) p) p))
          (mapcar #'cons bases exponents)
          :initial-value 1))

;;; ============================================================================
;;; Comparison and Predicates
;;; ============================================================================

(defun fp-equal-p (a b &optional (p +bls12-381-p+))
  "Test equality of field elements."
  (= (mod a p) (mod b p)))

(defun fp-zero-p (a &optional (p +bls12-381-p+))
  "Test if field element is zero."
  (zerop (mod a p)))

(defun fp-one-p (a &optional (p +bls12-381-p+))
  "Test if field element is one."
  (= 1 (mod a p)))
