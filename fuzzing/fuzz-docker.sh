#!/usr/bin/env bash

set -e

[[ -d ./fuzz-results ]] || mkdir -p ./fuzz-results

# --build-arg "cflags=-g" \
# --build-arg build_libcxx=yes \
sudo docker build -t llvm-pretty-bc-parser .
# --mount type=bind,source="$(pwd)"/out,target=/out \
sudo docker run --rm -it \
      --mount type=bind,source="$(realpath ./fuzz-results)",destination=/opt/fuzz-results \
      --mount type=bind,source="$(realpath ../)",destination=/opt/llvm-pretty-bc-parser,readonly \
       llvm-pretty-bc-parser
