# Additional flavour to your Emacs Lisp programming.

`el-spice` provides additional configuration to make programming in Emacs Lisp more enjoyable.

 * turns on `eldoc` for all emacs lisp files
 * adds functions to provide contextual help. Just type `C-c h` (Richard Riley)
 * adds helper functions for debugging and reading code (Helmut Eller)
 
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

# How to Install

`el-spice` requires [thingatpt+](http://www.emacswiki.org/emacs/thingatpt+.el). 
Install `thingatpt+` and add the following code to your init file:

```emacs-lisp
(add-to-list 'load-path "/path/to/el-spice/")
(require 'el-spice)
```
