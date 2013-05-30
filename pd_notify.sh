#!/bin/bash
PORT=12345
CMD="load $1;"
echo "Notify Pd (port $PORT): $CMD"
echo "$CMD" | netcat -q0 localhost $PORT
exit 0
