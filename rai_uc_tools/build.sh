#!/bin/bash
RAI_UC_TOOLS=$(dirname $0)
. $RAI_UC_TOOLS/env.sh
exec $UC_TOOLS/linux/build.sh
