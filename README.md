# Additional flavour to your Emacs Lisp programming.

`el-spice` is a minor mode that provides additional configuration to make programming in Emacs Lisp more enjoyable.

 * turns on `eldoc`
 * adds functions to provide contextual help. Just type `C-c C-?` (Richard Riley)
 * adds helper functions for debugging and reading emacs-lisp code (Helmut Eller)

   `C-c C-d`   - Disassemble elisp
   `C-c C-m`   - Macroexpand elisp
   `C-c M-m`   - Macroexpand all
   `C-c C-c` - Compile defun
   `C-c C-k` - Eval buffer
   `C-c l` - Load file
   `C-c C-p`   - Pretty-print Eval last sexp
   `M-.`     - Find Definition
   `M-,`     - Pop tag mark
   `C-c C-l`   - List the callers of this function
   `C-c <`   - Show function callers and callee summary for a package
   `C-c C-?`   - Show contextual help
   `C-c C-z` - easy-key-binding for \*ielm\*

# How to Install

## Package managers
`el-spice` is available for install from the package managers [Melpa](http://melpa.milkbox.net/) and [El-Get](http://github.com/dimitri/el-get/).

## Manual Install
Clone this repository and add the following code to your init file:

```emacs-lisp
(add-to-list 'load-path "/path/to/el-spice/")
```

# Configuration

Toggle `el-spice` activation with `M-x el-spice-mode` inside an elisp file. To activate `el-spice` for all emacs lisp programming (recommended), add the following lines to your .emacs file.

```emacs-lisp
(add-hook 'emacs-lisp-mode-hook 'el-spice-mode)
(add-hook 'lisp-interaction-mode-hook 'el-spice-mode)
```
