

EXT="${2##*.}"
BN=$(basename $2 .$EXT)
RKT=$BN.rkt
C=$(ls ../src/*.c ../src/*.h)
redo-ifchange $RKT $C
make -C $(readlink -f .) $2 >&2
mv $2 $3

