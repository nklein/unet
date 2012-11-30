
(defpackage :unet-utils-let-gensyms
  (:use :cl)
  (:export :let-gensyms))

(in-package :unet-utils-let-gensyms)

(defun ensure-symbol-value-pair (entry)
  (if (listp entry)
      entry
      (list entry nil)))

(defun gensym-for (sym)
  (gensym (concatenate 'string (symbol-name sym) "-")))
           
(defun build-lets-and-body (vars lets body)
  (cond
    ((null vars) (list (nreverse lets) body))
    (t (destructuring-bind (var value)
           (ensure-symbol-value-pair (first vars))
         (let ((gsym (gensym-for var)))
           (build-lets-and-body (rest vars)
                                (list* `(,gsym ,value) lets)
                                (subst gsym var body)))))))

;;; Exported macro let-gensyms
(defmacro let-gensyms (syms-and-values &body body)
  (destructuring-bind (lets body) (build-lets-and-body syms-and-values
                                                       nil
                                                       body)
    `(let ,lets ,@body)))
