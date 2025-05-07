#!/usr/bin/env bash

set -e

# This script is supposed to run your compiler
if [ "$HOSTNAME" == "nixOS" ]; then
landrun --unrestricted-network --rox /nix/store --rw /tmp --rw /dev --rw ~/.config/cabal --rw ~/.cache/cabal --rw ~/.local/state/cabal --rwx $(pwd) --env PATH cabal run l1c -- $@
else
cabal run l1c -- $@
fi
