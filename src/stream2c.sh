#!/bin/sh
# Driver script to evaluate stream language module to C code module.
[ -z "$2" ] && echo "usage: $0 <in.rkt> <out.c>" && exit 1
cat <<EOF | $RACKET -e '(eval (read))' | tee "$2"
(begin
  (require rai/ai-array-c)
  (require (file "$1"))
  (display (ai-array-c main
                 #:defaults main-defaults
                 #:nsi main-nsi
                 )))
EOF
