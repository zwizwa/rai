#!/usr/bin/env bash

# Idempotent install script for rai dependencies.

# This is work in progress.  Currently I'm not to clear about how I
# want to set this up.  Two things I want:
# - Make it straightforward to install from standard Racket channels
# - Integrate it in Exo Nix install


# Assumes "raco" is in the path.

# FIXME: Currently in the process of making this run inside of
# "nix-shell --pure"

# 1. Allow override RACKET_DIR
if [ ! -z "$RACKET_DIR" ]; then
    echo "using RACKET_DIR=$RACKET_DIR"
    RACKET=$RACKET_DIR/bin/racket
fi

# 2. Otherwise git it from path
[ -z "$RACKET" ] && RACKET=$(which racket)

[ -z "$RACKET" ] && echo "Can't locate racket" && exit 1

echo "racket is $(readlink -f $(which $RACKET))"
$RACKET --version

# 3. Assume raco is next to it
RACO=$(dirname $RACKET)/raco

# 4. Where to put deps?

[ -z "$RACKET_DIR" ] && RACKET_DIR=$(dirname $(dirname $RACKET))
echo "RACKET_DIR=$RACKET_DIR"

HERE_ABS=$(readlink -f $(dirname $0))
PARENT_ABS=$(dirname $HERE_ABS)
PRJ_DIR=$(basename $HERE_ABS)

echo "rai is in $HERE_ABS"

# FIXME: special cased.  Find a way to configure this.
if [ "/home/tom/.nix-profile" == $RACKET_DIR ]; then

    DEPS_DIR="${PARENT_ABS}/.${PRJ_DIR}.deps"
    echo "installing deps in $DEPS_DIR"
    mkdir -p ${DEPS_DIR}

cat <<EOF >with-env.sh
#!/usr/bin/env bash
# Generated by $0
# Set HOME, so user install goes in ${DEPS_DIR}/.racket
# Note that because of linking, the .racket dir cannot go under the rai/ dir.
HOME=${DEPS_DIR}
export PATH=${RACKET_DIR}/bin:\$PATH
racket --version >&2
exec "\$@"
EOF

else

cat <<EOF >with-env.sh
#!/usr/bin/env bash
export PATH=${RACKET_DIR}/bin:\$PATH
racket --version >&2
exec "\$@"
EOF

fi

cd ${HERE_ABS}

chmod +x with-env.sh
echo "generated:"
echo
cat with-env.sh

echo


echo "installing deps"

( ./with-env.sh $RACO pkg install --deps search-auto rsound )
( cd ${PARENT_ABS} ; ./${PRJ_DIR}/with-env.sh  $RACO pkg install --link rai )



exit 0
