with import <nixpkgs> { };

llvmPackages_6.libcxx.overrideAttrs (oldAttrs: {
  stdenv      = llvmPackages_6.stdenv;
  buildInputs = [clang] ++ oldAttrs.buildInputs;
  cmakeFlags  = "-DCMAKE_CXX_FLAGS=-save-temps -DCMAKE_CXX_COMPILER=clang++";

  installPhase = ''
    mkdir -p $out
    cp lib/*.bc $out
  '';
})
