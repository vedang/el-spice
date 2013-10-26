;;; el-spice.el --- Extra spice for emacs lisp programming
;;
;;; Copyright (C) 2013 Vedang Manerikar
;;
;; Author: Vedang Manerikar <vedang.manerikar@gmail.com>
;; Created on: 26 Oct 2013
;; Keywords: configuration
;; URL: https://github.com/vedang/el-spice
;; Package-Requires: ((thingatpt+)) ; update #2171
;; Version: DEV

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'eldoc)
(require 'thingatpt+)
(require 'list-callers)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'ielm)

;; From Emacswiki: Better context help
(defun rgr/toggle-context-help()
  "Turn on or off the context help.
Note that if ON and you hide the help buffer then you need to
manually reshow it. A double toggle will make it reappear"
  (interactive)
  (with-current-buffer (help-buffer)
    (unless (local-variable-p 'context-help)
      (set (make-local-variable 'context-help) t))
    (when (setq context-help (not context-help))
      (progn
        (when (not (get-buffer-window (help-buffer)))
          (display-buffer (help-buffer)))))
    (message "Context help %s" (if context-help "ON" "OFF"))))

(defun rgr/context-help()
  "Display function or variable at point in *Help* buffer if visible.
Default behaviour can be turned off by setting the buffer local
context-help to false"
  ;; symbol-at-point http://www.emacswiki.org/cgi-bin/wiki/thingatpt%2B.el
  (interactive)
  (let ((rgr-symbol (symbol-at-point)))
    (with-current-buffer (help-buffer)
      (unless (local-variable-p 'context-help)
        (set (make-local-variable 'context-help) t))
      (when (and context-help (get-buffer-window (help-buffer)) rgr-symbol)
        (if (fboundp  rgr-symbol)
            (describe-function rgr-symbol)
          (if (boundp  rgr-symbol) (describe-variable rgr-symbol)))))))

(defadvice eldoc-print-current-symbol-info
  (around eldoc-show-c-tag activate)
  (cond
   ((eq major-mode 'emacs-lisp-mode) (rgr/context-help) ad-do-it)
   ((eq major-mode 'lisp-interaction-mode) (rgr/context-help) ad-do-it)
   (t ad-do-it)))

(define-key emacs-lisp-mode-map (kbd "C-c h") 'rgr/toggle-context-help)

;; From the configuration of Helmut Eller

(defun helmut/elisp-disassemble (function)
  (interactive (list (function-called-at-point)))
  (disassemble function))


(defun helmut/elisp-pp (sexp)
  (with-output-to-temp-buffer "*Pp Eval Output*"
    (pp sexp)
    (with-current-buffer standard-output
      (emacs-lisp-mode))))


(defun helmut/elisp-macroexpand (form)
  (interactive (list (form-at-point 'sexp)))
  (helmut/elisp-pp (macroexpand form)))

(defun helmut/elisp-macroexpand-all (form)
  (interactive (list (form-at-point 'sexp)))
  (helmut/elisp-pp (cl-macroexpand-all form)))

(defun helmut/elisp-push-point-marker ()
  (require 'etags)
  (ring-insert find-tag-marker-ring (point-marker)))


(defun helmut/elisp-find-definition (name)
  "Jump to the definition of the function (or variable) at point."
  (interactive (list (thing-at-point 'symbol)))
  (cond (name
         (let ((symbol (intern-soft name))
               (search (lambda (fun sym)
                         (let* ((r (save-excursion (funcall fun sym)))
                                (buffer (car r))
                                (point (cdr r)))
                           (cond ((not point)
                                  (error "Found no definition for %s in %s"
                                         name buffer))
                                 (t
                                  (switch-to-buffer buffer)
                                  (goto-char point)
                                  (recenter 1)))))))
           (cond ((fboundp symbol)
                  (helmut/elisp-push-point-marker)
                  (funcall search 'find-function-noselect symbol))
                 ((boundp symbol)
                  (helmut/elisp-push-point-marker)
                  (funcall search 'find-variable-noselect symbol))
                 (t
                  (message "Symbol not bound: %S" symbol)))))
        (t (message "No symbol at point"))))

(defvar helmut/elisp-extra-keys
  '(((kbd "C-c d")   'helmut/elisp-disassemble)
    ((kbd "C-c m")   'helmut/elisp-macroexpand)
    ((kbd "C-c M")   'helmut/elisp-macroexpand-all)
    ((kbd "C-c C-c") 'compile-defun)
    ((kbd "C-c C-k") 'eval-buffer)
    ((kbd "C-c C-l") 'load-file)
    ((kbd "C-c p")   'pp-eval-last-sexp)
    ((kbd "M-.")     'helmut/elisp-find-definition)
    ((kbd "M-,")     'pop-tag-mark)
    ((kbd "C-c <")   'list-callers)))


(dolist (binding helmut/elisp-extra-keys)
  (let ((key (eval (car binding))) (val (eval (cadr binding))))
    (define-key emacs-lisp-mode-map key val)
    (define-key lisp-interaction-mode-map key val)))


(provide 'el-spice)
