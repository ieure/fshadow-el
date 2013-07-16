;;; fshadow.el --- shadowed flet

;; Copyright (C) 2013  Ian Eure

;; Author: Ian Eure <ian.eure@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile
  (require 'cl))

(defun fshadow-save-sym (func)
  "Return a symbol to save FUNC's definition in."
  (make-symbol (concat "orig--" (symbol-name func))))

(defun fshadow-save (funcs)
  "Generate bindings to save defs of FUNCS."
  (mapcar (lambda (func)
            (list func
                  (fshadow-save-sym func)
                  (symbol-function func)))
          funcs))

(defun fshadow* (saved binding)
  "Wrap `flet' binding BINDING so the shadowed function is available."
  (let* ((sym (car binding))
         (orig (cadr (assoc sym saved)))
         (args (cadr binding))
         (body (cddr binding)))
    `(,sym ,args (flet ((this-fn (&rest args)
                                 (apply 'funcall ,orig args)))
                   ,@body))))

(defun fshadow* (saved binding)
  "Wrap `flet' binding BINDING so the shadowed function is available."
  (let* ((sym (car binding))
         (orig (cadr (assoc sym saved)))
         (args (cadr binding))
         (body (cddr binding)))
    `(,sym ,args (flet ((this-fn (&rest args)
                                 (apply 'funcall ,orig args)))
                   ,@body))))

(defmacro fshadow (bindings &rest body)
  "Enhanced `cl-flet' with access to shadowed functions.

   fshadow works exactly like `cl-flet', except, within the context of
   the temporary function definition, the symbol `this-fn' refers to
   the original function."
  (let ((saved (fshadow-save (mapcar 'car bindings))))
    `(let ,(mapcar 'cdr saved)
       (cl-flet ,(mapcar (lambda (binding) (fshadow* saved binding)) bindings)
         ,@body))))

(put 'fshadow 'lisp-indent-function 1)

(provide 'fshadow)
;;; fshadow.el ends here
