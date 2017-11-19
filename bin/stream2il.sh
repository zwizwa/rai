#!/usr/bin/env bash
# Driver script to evaluate stream language module to C code module.
[ -z "$2" ] && echo "usage: $0 <in.rkt> <out.il>" && exit 1
cat <<EOF | racket -e '(eval (read))' > "$2"
(begin
  (require rai/ai-array)
  (require (file "$1"))
  (pretty-print (ai-array main
                 #:nsi main-nsi
                 )))
EOF
