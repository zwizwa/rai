#!/bin/sh
# Driver script to evaluate stream language module to C code module.

# This writes stdout to a file so take some precautions: delete
# existing file, write to temporary output, and only move file into
# place if there are no errors.

set -e
[ -z "$2" ] && echo "usage: $0 <in.rkt> <out.c>" && exit 1
[ -z "$RACKET" ] && echo "need RACKET" && exit 1
TMP="$2.tmp"
rm -f "$2" "$TMP"
cat <<EOF | $RACKET -e '(eval (read))'
(begin
  (require rai/ai-array-c)
  (require (file "$1"))
  (let ((out (open-output-file "$TMP")))
    (display
      (ai-array-c main
                  #:defaults main-defaults
                  #:nsi main-nsi)
      out)
    (close-output-port out)))
EOF
mv "$TMP" "$2"
ls -l "$2"
# cat "$2"
