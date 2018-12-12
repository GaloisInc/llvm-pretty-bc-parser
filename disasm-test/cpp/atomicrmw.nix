with import <nixpkgs> { };

let libcxxbc = import ./libcxxbc.nix;
in llvmPackages_6.stdenv.mkDerivation {
  name = "atomicrmw";
  buildInputs = [ clang llvm_6 ];
  src = lib.sourceFilesBySuffices ./. [ ".cpp" ];
  buildPhase = ''
    clang++ -emit-llvm -g -O0 -o atomicrmw.bc -c atomicrmw.cpp
    llvm-link -only-needed ${libcxxbc}/*.bc atomicrmw.bc > atomicrmw.bc
  '';
  installPhase = ''
    mkdir -p $out
    cp *.bc $out/
  '';

}
