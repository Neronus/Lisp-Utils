#!/bin/sh

../buildapp-1.1/buildapp --load "shelisp" \
         --load-system "cl-fad" \
         --load "randBG" \
         --output "utils" \
         --dispatched-entry "randBG/randBG:main"
