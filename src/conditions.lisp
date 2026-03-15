;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-finite-fields)

(define-condition cl-finite-fields-error (error)
  ((message :initarg :message :reader cl-finite-fields-error-message))
  (:report (lambda (condition stream)
             (format stream "cl-finite-fields error: ~A" (cl-finite-fields-error-message condition)))))
