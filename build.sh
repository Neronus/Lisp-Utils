#!/bin/sh

../buildapp-1.1/buildapp --load "shelisp" \
         --load-system "cl-fad" \
         --load "$HOME/quicklisp/setup" \
         --eval "(ql:quickload \"drakma\")" \
         --eval "(ql:quickload \"cl-ppcre\")" \
         --load "utils.lisp" \
         --load "randBG" \
         --load "cat" \
         --load "InterfaceLisp" \
         --output "utils" \
         --dispatched-entry "randBG/randBG:main" \
         --dispatched-entry "mycat/cat:main" \
         --dispatched-entry "InterfaceLift/interface-lift:main"
         
