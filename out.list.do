elfs () {
    echo test/$1.pulse.host.elf
    echo test/$1.jack.host.elf
}

cat <<EOF >$3
$(elfs controlspace)
$(elfs synth)
$(elfs doodle)
EOF
