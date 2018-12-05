# -*- mode: conf -*-
# * Dockerized fuzzing

# ** Base image: [[https://hub.docker.com/r/nixos/nix/][nixos/nix]]

FROM nixos/nix AS base

# Allow getting GHC from unstable
RUN nix-channel --add https://nixos.org/channels/nixpkgs-unstable unstable
RUN nix-channel --update

# Show available GHC versions
RUN nix-env -qaP -A unstable.haskell.compiler
RUN nix-env -iA unstable.haskellPackages.cabal-install \
                unstable.haskell.compiler.ghc844

COPY    shell.nix /opt/shell.nix
WORKDIR           /opt/llvm-pretty-bc-parser

# This line caches the packages being added to nix store
RUN nix-shell --pure ../shell.nix --run "exit 0"

# ** Build

FROM base

# Unfortunately, when building executables, GHC will ignore the builddir argument
# and fail at link-time. Thus, we have to copy the source directory rather than
# set --builddir/--destdir.
CMD nix-shell --pure ../shell.nix \
      --run "ls -alh      && \
             cp -r $(pwd) /tmp/build/ && \
             cd           /tmp/build/ && \
             cabal update && \
             cabal -j$(nproc) new-build disasm-test && \
             cd fuzzing && ./fuzz.sh -k -n 5 -w /opt/fuzz-results"