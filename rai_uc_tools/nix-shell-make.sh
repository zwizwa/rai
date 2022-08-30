#!/bin/sh
exec nix-shell -p gcc libjack2 --run make
