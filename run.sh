#!/usr/bin/env bash

set -e

export LC_CTYPE=C.UTF-8 # Why doesn't Haskell default to utf-8 ??????

# This script is supposed to run your compiler
if [ "$HOSTNAME" == "nixOS" ]; then
landrun --unrestricted-network --rox /nix/store --rw /tmp --rw /dev --rw ~/.config/cabal --rw ~/.cache/cabal --rw ~/.local/state/cabal --rwx $(pwd) --env PATH --env LC_CTYPE cabal run l1c -- $@
else
cabal run l1c -- $@
fi
