;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

(load "cl-finite-fields.asd")
(handler-case
  (progn
    (asdf:test-system :cl-finite-fields/test)
    (format t "PASS~%"))
  (error (e)
    (format t "FAIL~%")))
(quit)
