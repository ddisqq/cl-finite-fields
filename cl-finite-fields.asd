;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

;;;; cl-finite-fields.asd
;;;; Finite field arithmetic for cryptographic applications

(asdf:defsystem #:cl-finite-fields
  :description "Pure Common Lisp finite field arithmetic for cryptography"
  :author "Park Ian Co"
  :license "Apache-2.0"
  :version "0.1.0"
  :serial t
  :components ((:file "package")
               (:module "src"
                :components ((:file "package")
                             (:file "conditions" :depends-on ("package"))
                             (:file "types" :depends-on ("package"))
                             (:file "cl-finite-fields" :depends-on ("package" "conditions" "types"))))))
  :in-order-to ((asdf:test-op (test-op #:cl-finite-fields/test))))

(asdf:defsystem #:cl-finite-fields/test
  :description "Tests for cl-finite-fields"
  :depends-on (#:cl-finite-fields)
  :serial t
  :components ((:module "test"
                :serial t
                :components ((:file "field-test"))))
  :perform (asdf:test-op (o c)
             (let ((result (uiop:symbol-call :cl-finite-fields/test '#:run-tests)))
               (unless result
                 (error "Tests failed")))))
