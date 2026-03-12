;;;; cl-finite-fields.asd
;;;; Finite field arithmetic for cryptographic applications

(asdf:defsystem #:cl-finite-fields
  :description "Pure Common Lisp finite field arithmetic for cryptography"
  :author "Parkian Company LLC"
  :license "BSD-3-Clause"
  :version "1.0.0"
  :serial t
  :components ((:file "package")
               (:module "src"
                :serial t
                :components ((:file "field"))))
  :in-order-to ((test-op (test-op #:cl-finite-fields/test))))

(asdf:defsystem #:cl-finite-fields/test
  :description "Tests for cl-finite-fields"
  :depends-on (#:cl-finite-fields)
  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "field-test"))))
  :perform (test-op (o c)
             (let ((result (uiop:symbol-call :cl-finite-fields/test '#:run-tests)))
               (unless result
                 (error "Tests failed")))))
