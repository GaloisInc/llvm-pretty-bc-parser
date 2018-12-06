#!/usr/bin/env bash

set -e

url="https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.03.tar.gz"

for f in *.cpp; do
  nix-shell --pure \
            -p "with import (fetchTarball $url) {}; llvm_6" \
            -p "with import (fetchTarball $url) {}; clang_6" \
            --run "clang++ -O0 -g -emit-llvm -c $f -o ${f%.cpp}.bc && \
                   llvm-dis -o ${f%.cpp}.ll ${f%.cpp}.bc && \
                   llvm-bcanalyzer -dump ${f%.cpp}.bc > ${f%.cpp}.xml"
done
