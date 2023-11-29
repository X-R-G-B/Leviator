#!/bin/bash

if command -v apt-get 2>/dev/null; then
    apt-get update
    apt-get install -y make
elif command -v pacman 2>/dev/null; then
    pacman -Sy make
elif command -v dnf 2>/dev/null; then
    dnf update
    dnf install -y make
else
    echo "Unsupported package manager"
    echo "Please install 'make'"
    exit 12
fi
