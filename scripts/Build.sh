#!/bin/bash

BIN_STACK="$1"
BIN_TARGET="$2"

stack build

cp "$(stack path --local-install-root)/bin/$BIN_STACK" "$BIN_TARGET"
