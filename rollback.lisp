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

(defpackage :rollback
  (:use :cl)
  (:export #:rollback-function
           #:rollback
           #:with-rollback
           #:with-rollback*))

(in-package :rollback)

(defun rollback-function (op)
  (get op 'rollback-function))

(defun set-rollback-function (op rollback-fn)
  (setf (get op 'rollback-function) rollback-fn))

(defsetf rollback-function set-rollback-function)

(defun rollback (op &rest args)
  (let ((rollback-fn (rollback-function op)))
    (unless rollback-fn
      (error "Undefined rollback function for ~S" op))
    (apply rollback-fn args)))

(defmacro with-rollback ((fun &rest args) &body body)
  (let ((rollback (gensym "ROLLBACK-"))
        (g!args (mapcar (lambda (x)
                          (declare (ignore x))
                          (gensym "ARG-"))
                        args)))
    `(let ((,rollback t)
           ,@(loop
                for var in g!args
                for value in args
                collect `(,var ,value)))
       (,fun ,@g!args)
       (unwind-protect (prog1 ,(if (= 1 (length body))
                                   (car body)
                                   `(progn ,@body))
                         (setf ,rollback nil))
         (when ,rollback
           (rollback ',fun ,@g!args))))))

(defmacro with-rollback* (&body forms)
  (reduce (lambda (body form)
            (if body
                `(with-rollback ,form
                   ,body)
                form))
          (reverse forms)))
