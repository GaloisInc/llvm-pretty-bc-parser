{ pkgs          ? import <unstable> { }
, ghc-version   ? "ghc844"
, clang         ? pkgs.clang_38
, llvm          ? pkgs.llvm_38
}:

let self = pkgs.haskellPackages.callCabal2nix "name" ./. { };
in with pkgs; stdenv.mkDerivation {
  name = "llvm-pretty-bc-parser-fuzzing";
  src = if lib.inNixShell then null else lib.sourceFilesBySuffices ./. [ ".cabal" ".hs" ];
  shellHook = ''
    export CSMITH_PATH=$(cd ${csmith}/include/ && realpath $(ls | head -n 1))
  '';
  buildInputs = [
    haskellPackages.cabal-install
    haskell.compiler.${ghc-version}

    csmith
    clang
    llvm
  ];

}
