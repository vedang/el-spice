# Additional flavour to your Emacs Lisp programming.

`el-spice` is a minor mode that provides additional configuration to make programming in Emacs Lisp more enjoyable.

 * turns on `eldoc`
 * adds functions to provide contextual help. Just type `C-c h` (Richard Riley)
 * adds helper functions for debugging and reading emacs-lisp code (Helmut Eller)
 
   `C-c d`   - Disassemble elisp  
   `C-c m`   - Macroexpand elisp  
   `C-c M`   - Macroexpand all  
   `C-c C-c` - Compile defun  
   `C-c C-k` - Eval buffer  
   `C-c C-l` - Load file  
   `C-c p`   - Pretty-print Eval last sexp  
   `M-.`     - Find Definition  
   `M-,`     - Pop tag mark  
   `C-c <`   - List the callers of this function  
   `C-c h`   - Show contextual help  
   `C-c C-z` - easy-key-binding for \*ielm\*   

# How to Install

`el-spice` requires [thingatpt+](http://www.emacswiki.org/emacs/thingatpt+.el). 

Install `thingatpt+` and add the following code to your init file:

```emacs-lisp
(add-to-list 'load-path "/path/to/el-spice/")
(add-hook 'emacs-lisp-mode-hook 'el-spice-mode)
(add-hook 'lisp-interaction-mode-hook 'el-spice-mode)
```
