;;; el-spice.el --- Extra spice for emacs lisp programming
;;
;;; Copyright (C) 2013 Vedang Manerikar
;;
;; Author: Vedang Manerikar <vedang.manerikar@gmail.com>
;; Created on: 26 Oct 2013
;; Keywords: configuration
;; URL: https://github.com/vedang/el-spice
;; Package-Requires: ((thingatpt+)) ; update #2171
;; Version: 0.2.0
;;
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
;;
;; Commentary:
;; Refer to installation instructions in the README document.
;;
;;; Code:

(require 'eldoc)
(require 'thingatpt+)
(require 'list-callers)


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
  (around eldoc-show-c-tag disable)
  (cond
   ((eq major-mode 'emacs-lisp-mode) (rgr/context-help) ad-do-it)
   ((eq major-mode 'lisp-interaction-mode) (rgr/context-help) ad-do-it)
   (t ad-do-it)))


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


(defvar el-spice-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c d") 'helmut/elisp-disassemble)
    (define-key map (kbd "C-c m") 'helmut/elisp-macroexpand)
    (define-key map (kbd "C-c M") 'helmut/elisp-macroexpand-all)
    (define-key map (kbd "C-c C-c") 'compile-defun)
    (define-key map (kbd "C-c C-k") 'eval-buffer)
    (define-key map (kbd "C-c C-l") 'load-file)
    (define-key map (kbd "C-c p") 'pp-eval-last-sexp)
    (define-key map (kbd "M-.") 'helmut/elisp-find-definition)
    (define-key map (kbd "M-,") 'pop-tag-mark)
    (define-key map (kbd "C-c <") 'list-callers)
    (define-key map (kbd "C-c h") 'rgr/toggle-context-help)
    (define-key map (kbd "C-c C-z") 'ielm)
    map))


;;;###autoload
(define-minor-mode el-spice-mode
  "Extra spice for emacs lisp programming.

With a prefix argument ARG, enables el-spice mode if ARG is
positive, and disables it otherwise. If called from Lisp,
disables the mode if the argument is a non-positive integer, and
enables the mode otherwise (including if the argument is omitted
or nil or a positive integer)."
  :lighter " ElS"
  :keymap el-spice-mode-map
  (if el-spice-mode
      (progn
        (eldoc-mode +1)
        (ad-enable-advice 'eldoc-print-current-symbol-info
                          'around
                          'eldoc-show-c-tag)
        (ad-activate 'eldoc-print-current-symbol-info))
    (progn
      (eldoc-mode -1)
      (ad-disable-advice 'eldoc-print-current-symbol-info
                         'around
                         'eldoc-show-c-tag)
      (ad-activate 'eldoc-print-current-symbol-info))))


(provide 'el-spice)
