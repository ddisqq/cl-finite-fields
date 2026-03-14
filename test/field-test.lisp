;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

;;;; field-test.lisp
;;;; Tests for cl-finite-fields

(defpackage #:cl-finite-fields/test
  (:use #:cl #:cl-finite-fields)
  (:export #:run-tests))

(in-package #:cl-finite-fields/test)

(defvar *test-count* 0)
(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defmacro deftest (name &body body)
  `(progn
     (incf *test-count*)
     (handler-case
         (progn
           ,@body
           (incf *pass-count*)
           (format t "  PASS: ~A~%" ',name))
       (error (e)
         (incf *fail-count*)
         (format t "  FAIL: ~A~%        ~A~%" ',name e)))))

(defmacro assert-equal (expected actual)
  `(unless (equal ,expected ,actual)
     (error "Expected ~A but got ~A" ,expected ,actual)))

(defmacro assert-true (expr)
  `(unless ,expr
     (error "Expected true but got false: ~A" ',expr)))

(defun run-tests ()
  (setf *test-count* 0 *pass-count* 0 *fail-count* 0)
  (format t "~%Running cl-finite-fields tests...~%~%")

  ;; Basic Fp arithmetic
  (format t "Fp Arithmetic:~%")

  (deftest fp-add-basic
    (let ((p 17))
      (assert-equal 10 (fp-add 5 5 p))
      (assert-equal 3 (fp-add 10 10 p))))  ; 20 mod 17 = 3

  (deftest fp-sub-basic
    (let ((p 17))
      (assert-equal 5 (fp-sub 10 5 p))
      (assert-equal 12 (fp-sub 5 10 p))))  ; -5 mod 17 = 12

  (deftest fp-mul-basic
    (let ((p 17))
      (assert-equal 8 (fp-mul 4 2 p))
      (assert-equal 9 (fp-mul 3 3 p))))

  (deftest fp-neg-basic
    (let ((p 17))
      (assert-equal 12 (fp-neg 5 p))
      (assert-equal 0 (fp-neg 0 p))))

  (deftest fp-square-basic
    (let ((p 17))
      (assert-equal 9 (fp-square 3 p))
      (assert-equal 16 (fp-square 4 p))))

  (deftest fp-pow-basic
    (let ((p 17))
      (assert-equal 1 (fp-pow 5 0 p))
      (assert-equal 5 (fp-pow 5 1 p))
      (assert-equal 8 (fp-pow 2 3 p))
      (assert-equal 16 (fp-pow 2 4 p))))

  (deftest fp-inv-basic
    (let ((p 17))
      (assert-equal 1 (fp-mul 3 (fp-inv 3 p) p))
      (assert-equal 1 (fp-mul 7 (fp-inv 7 p) p))))

  (deftest fp-div-basic
    (let ((p 17))
      (assert-equal 5 (fp-div 10 2 p))
      (let ((result (fp-div 7 3 p)))
        (assert-equal 7 (fp-mul result 3 p)))))

  ;; Square roots
  (format t "~%Square Roots:~%")

  (deftest fp-sqrt-basic
    (let ((p 17))
      (let ((sqrt-4 (fp-sqrt 4 p)))
        (assert-true (member (fp-square sqrt-4 p) '(4)))))
    (let ((p 23))
      (let ((sqrt-9 (fp-sqrt 9 p)))
        (assert-equal 9 (fp-square sqrt-9 p)))))

  (deftest fp-legendre-symbol-test
    (let ((p 17))
      (assert-equal 0 (fp-legendre-symbol 0 p))
      (assert-equal 1 (fp-legendre-symbol 1 p))
      (assert-equal 1 (fp-legendre-symbol 4 p))))

  ;; Batch operations
  (format t "~%Batch Operations:~%")

  (deftest fp-batch-inv-test
    (let* ((p 17)
           (elements '(2 3 5 7))
           (inverses (fp-batch-inv elements p)))
      (loop for e in elements
            for inv in inverses
            do (assert-equal 1 (fp-mul e inv p)))))

  ;; Fp2 operations
  (format t "~%Fp2 Operations:~%")

  (deftest fp2-add-test
    (let ((a (make-fp2-element 3 5))
          (b (make-fp2-element 7 2)))
      (let ((c (fp2-add a b)))
        (assert-equal (fp-add 3 7) (fp2-element-c0 c))
        (assert-equal (fp-add 5 2) (fp2-element-c1 c)))))

  (deftest fp2-mul-test
    ;; (3 + 5u)(7 + 2u) = 21 - 10 + (6 + 35)u = 11 + 41u (mod p)
    (let ((a (make-fp2-element 3 5))
          (b (make-fp2-element 7 2)))
      (let ((c (fp2-mul a b)))
        (assert-true (fp2-element-p c)))))

  (deftest fp2-inv-test
    (let ((a (make-fp2-element 3 5)))
      (let* ((a-inv (fp2-inv a))
             (product (fp2-mul a a-inv)))
        (assert-true (fp2-one-p product)))))

  (deftest fp2-conjugate-test
    (let ((a (make-fp2-element 3 5)))
      (let ((conj (fp2-conjugate a)))
        (assert-equal 3 (fp2-element-c0 conj))
        (assert-equal (fp-neg 5) (fp2-element-c1 conj)))))

  ;; Serialization
  (format t "~%Serialization:~%")

  (deftest fp-roundtrip-test
    (let ((val 12345678901234567890))
      (let* ((bytes (fp-to-bytes val 32))
             (recovered (fp-from-bytes bytes)))
        (assert-equal (mod val +bls12-381-p+) recovered))))

  (deftest fp-hex-roundtrip
    (let ((val 255))
      (let* ((hex (fp-to-hex val))
             (recovered (fp-from-hex hex)))
        (assert-equal val recovered))))

  ;; Montgomery
  (format t "~%Montgomery Multiplication:~%")

  (deftest montgomery-roundtrip
    (let* ((ctx (make-default-montgomery-context))
           (val 12345))
      (let* ((mont (mont-from-normal val ctx))
             (back (mont-to-normal mont ctx)))
        (assert-equal val back))))

  ;; Summary
  (format t "~%========================================~%")
  (format t "Tests: ~A  Passed: ~A  Failed: ~A~%"
          *test-count* *pass-count* *fail-count*)
  (format t "========================================~%~%")

  (zerop *fail-count*))
