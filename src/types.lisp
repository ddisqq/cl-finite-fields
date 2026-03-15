;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-finite-fields)

;;; Core types for cl-finite-fields
(deftype cl-finite-fields-id () '(unsigned-byte 64))
(deftype cl-finite-fields-status () '(member :ready :active :error :shutdown))
