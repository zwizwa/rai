#!/bin/sh
# Driver script to evaluate stream language module to C code module.
[ -z "$2" ] && echo "usage: $0 <in.rkt> <out.il>" && exit 1
cat <<EOF | $RACKET -e '(eval (read))' > "$2"
(begin
  (require "ai-array.rkt")
  (require (file "$1"))
  (pretty-print (ai-array main
                 #:nsi main-nsi
                 )))
EOF
