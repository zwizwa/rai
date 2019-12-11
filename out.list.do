elfs () {
    echo test/$1.pulse.host.elf
    echo test/$1.jack.host.elf
}

rktelfs () {
    for rkt in test/2019*.rkt; do 
        elfs $(basename $rkt .rkt)
    done
}


cat <<EOF >$3
$(elfs controlspace)
$(elfs synth)
$(elfs doodle)
$(rktelfs)
EOF
