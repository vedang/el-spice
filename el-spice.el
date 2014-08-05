;;; el-spice.el --- Extra spice for emacs lisp programming
;;
;; Copyright (C) 2013 Helmut Eller, Richard Riley, Vedang Manerikar
;;
;; Author: Vedang Manerikar <vedang.manerikar@gmail.com>
;; Created on: 26 Oct 2013
;; Keywords: languages, extensions
;; URL: https://github.com/vedang/el-spice
;; Package-Requires: ((thingatpt+ "0"))
;; Version: 0.3.0-wip
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
;;; Commentary:
;;
;; Refer to installation instructions in the README document.
;;
;;; Code:

(require 'eldoc)
(require 'etags)
(require 'thingatpt+)
(require 'list-callers)


;; From Emacswiki: Better context help (Author: Richard Riley)

(defun el-toggle-context-help()
  "Turn on or off the context help.
Note that if ON and you hide the help buffer then you need to
manually reshow it. A double toggle will make it reappear."
  (interactive)
  (with-current-buffer (help-buffer)
    (unless (local-variable-p 'context-help)
      (set (make-local-variable 'context-help) t))
    (when (and (setq context-help (not context-help))
               (not (get-buffer-window (help-buffer))))
      (display-buffer (help-buffer)))
    (message "Context help %s" (if context-help "ON" "OFF"))))

(defun el-context-help()
  "Display function or variable at point in *Help* buffer if visible.
Default behaviour can be turned off by setting the buffer local
context-help to false"
  ;; tap-symbol-at-point http://www.emacswiki.org/cgi-bin/wiki/thingatpt%2B.el
  (interactive)
  (let ((rgr-symbol (tap-symbol-at-point)))
    (with-current-buffer (help-buffer)
      (unless (local-variable-p 'context-help)
        (set (make-local-variable 'context-help) t))
      (when (and context-help (get-buffer-window (help-buffer)) rgr-symbol)
        (if (fboundp rgr-symbol)
            (describe-function rgr-symbol)
          (when (boundp rgr-symbol)
            (describe-variable rgr-symbol)))))))

(defadvice eldoc-print-current-symbol-info
  (around eldoc-show-c-tag disable)
  (cond
   ((eq major-mode 'emacs-lisp-mode) (el-context-help) ad-do-it)
   ((eq major-mode 'lisp-interaction-mode) (el-context-help) ad-do-it)
   (t ad-do-it)))


;; From the configuration of Helmut Eller

(defun el-elisp-disassemble (function)
  "Show disassembly for the function under point, or the calling
function in the list under point."
  (interactive (list (function-called-at-point)))
  (disassemble function))

(defun el-elisp-pp (sexp)
  "Pretty-print the S-expression into a buffer called *Pp Eval Output*"
  (with-output-to-temp-buffer "*Pp Eval Output*"
    (pp sexp)
    (with-current-buffer standard-output
      (emacs-lisp-mode))))

(defun el-elisp-macroexpand (form)
  "Invoke 'macroexpand-1' on the expression at point."
  (interactive (list (tap-form-at-point 'sexp)))
  (el-elisp-pp (macroexpand form)))

(defun el-elisp-macroexpand-all (form)
  "Invoke 'macroexpand-all' on the expression at point."
  (interactive (list (tap-form-at-point 'sexp)))
  (el-elisp-pp (cl-macroexpand-all form)))

(defun el-elisp-push-point-marker ()
  (ring-insert find-tag-marker-ring (point-marker)))

(defun el-elisp-find-definition (name)
  "Jump to the definition of the function (or variable) at point."
  (interactive (list (tap-thing-at-point 'symbol)))
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
                  (el-elisp-push-point-marker)
                  (funcall search 'find-function-noselect symbol))
                 ((boundp symbol)
                  (el-elisp-push-point-marker)
                  (funcall search 'find-variable-noselect symbol))
                 (t
                  (message "Symbol not bound: %S" symbol)))))
        (t (message "No symbol at point"))))


(defvar el-spice-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-d") 'el-elisp-disassemble)
    (define-key map (kbd "C-c C-m") 'el-elisp-macroexpand)
    (define-key map (kbd "C-c M-m") 'el-elisp-macroexpand-all)
    (define-key map (kbd "C-c C-c") 'compile-defun)
    (define-key map (kbd "C-c C-k") 'eval-buffer)
    (define-key map (kbd "C-c C-l") 'load-file)
    (define-key map (kbd "C-c C-p") 'pp-eval-last-sexp)
    (define-key map (kbd "M-.") 'el-elisp-find-definition)
    (define-key map (kbd "M-,") 'pop-tag-mark)
    (define-key map (kbd "C-c C-l") 'list-callers)
    (define-key map (kbd "C-c <") 'lc-show-package-summary)
    (define-key map (kbd "C-c C-?") 'el-toggle-context-help)
    (define-key map (kbd "C-c C-z") 'ielm)
    map))


(defcustom el-spice-lighter " ElS"
  "Change this variable to nil if you don't want the el-spice
  lighter in your modeline."
  :type '(choice string (const :tag "None" nil)))


;;;###autoload
(define-minor-mode el-spice-mode
  "Extra spice for emacs lisp programming.

With a prefix argument ARG, enables el-spice mode if ARG is
positive, and disables it otherwise. If called from Lisp,
disables the mode if the argument is a non-positive integer, and
enables the mode otherwise (including if the argument is omitted
or nil or a positive integer)."
  :lighter el-spice-lighter
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
;;; el-spice.el ends here
