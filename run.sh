#!/usr/bin/env bash
set -x

# This script is supposed to run your compiler
[ "$HOSTNAME" == "nixOS" ] && landrun --unrestricted-network --rox /nix/store --rw /tmp --rw /dev --rw ~/.config/cabal --rw ~/.cache/cabal --rw ~/.local/state/cabal --rwx $(pwd) --env PATH cabal run l1c -- $@
[ "$HOSTNAME" != "nixOS" ] && cabal run l1c -- $@
