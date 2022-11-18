;; rollback - rollback functions
;; Copyright 2012-2022 Thomas de Grivel <thodg@kmx.io>
;;
;; Permission is hereby granted to use this software granted
;; the above copyright notice and this permission paragraph
;; are included in all copies and substantial portions of this
;; software.
;;
;; THIS SOFTWARE IS PROVIDED "AS-IS" WITHOUT ANY GUARANTEE OF
;; PURPOSE AND PERFORMANCE. IN NO EVENT WHATSOEVER SHALL THE
;; AUTHOR BE CONSIDERED LIABLE FOR THE USE AND PERFORMANCE OF
;; THIS SOFTWARE.

(defpackage :rollback.system
  (:use :cl :asdf))

(in-package :rollback.system)

(defsystem :rollback
  :name "rollback"
  :author "Thomas de Grivel <thodg@kmx.io>"
  :version "0.1"
  :description "rollback functions"
  :components
  ((:file "rollback")))
