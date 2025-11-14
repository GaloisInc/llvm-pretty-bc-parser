{
  # SEE NOTE when needing to change the set of LLVM tested versions.

  # nix build .
  # nix develop
  # nix run .  [runs llvm-disasm]
  #
  # nix run github:galoisinc/llvm-pretty-bc-parser   [runs llvm-disasm]

  description = "Flake to build the haskell-src package 'llvm-pretty-bc-parser' and dependencies";

  nixConfig.bash-prompt-suffix = "llvm-pretty-bc-parser.env} ";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs_old_llvm.url = "github:nixos/nixpkgs/23.05";
    nixpkgs_mid_llvm.url = "github:nixos/nixpkgs/25.05";
    levers = {
      url = "github:kquick/nix-levers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    llvm-pretty-src = {
      url = "github:GaloisInc/llvm-pretty";
      flake = false;
    };
    tasty-sugar = {
      url = "github:kquick/tasty-sugar";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.levers.follows = "levers";
    };
  };

  outputs = { self, levers, nixpkgs
            , nixpkgs_old_llvm
            , nixpkgs_mid_llvm
            , llvm-pretty-src
            , tasty-sugar
            }:
    let pkg_ghcvers = pkgs:
          # GHC 9.12 has an internal bug and fails when compiling llvm-pretty
          # (https://gitlab.haskell.org/ghc/ghc/-/issues/25771).  This bug is
          # fixed in GHC 9.12.3; remove the following ghcver setting when GHC
          # 9.12.3 is available in nixpkgs.
          builtins.filter
            (n: builtins.substring 0 6 n != "ghc912")
            (levers.validGHCVersions pkgs.haskell.compiler);
    in
    rec {

      devShells = levers.haskellShells
        { inherit nixpkgs;
          flake = self;
          defaultPkg = "llvm-pretty-bc-parser";
          additionalPackages = pkgs: [
            # Choose one of the sets below to choose which version of Clang/LLVM
            # you would like available in your development shell (or specify your
            # own version following these as a model):

            # pkgs.clang_17
            # pkgs.llvm_17

            # nixpkgs_old_llvm.legacyPackages.x86_64-linux.clang_16
            # nixpkgs_old_llvm.legacyPackages.x86_64-linux.llvm_16

            nixpkgs_mid_llvm.legacyPackages.x86_64-linux.clang_19
            nixpkgs_mid_llvm.legacyPackages.x86_64-linux.llvm_19

            # Other packages to add to the development shell:
            pkgs.cabal-install
            pkgs.csmith
          ];
          ghcvers = system: pkg_ghcvers (nixpkgs.legacyPackages.${system});
        };

      packages = levers.eachSystem (system:
        let
          mkHaskell = levers.mkHaskellPkg
            { inherit nixpkgs system;
              ghcver = pkg_ghcvers (nixpkgs.legacyPackages.${system});
            };
          pkgs = import nixpkgs { inherit system; };
          wrap = levers.pkg_wrapper system pkgs;

          # Runs the tests that were previously 'built', using the specified
          # version of LLVM and Clang.
          llvm-pretty-bc-parser-test = built: llvmver:
            let llvm = levers.get_pkg_at_ver system nixpkg_list "llvm_" llvmver;
                clang = levers.get_pkg_at_ver system nixpkg_list "clang_" llvmver;
                nixpkg_list = [
                  nixpkgs_mid_llvm # llvm 12-19
                  nixpkgs_old_llvm # llvm 5-16
                  nixpkgs # 18 and above (2025-10-27)
                ];
            in derivation {
              inherit system;
              name = "llvm-pretty-bc-parser-test-with-llvm${llvmver}";
              builder = "${pkgs.bash}/bin/bash";
              args = [ "-c"
                       ''
                       ${pkgs.coreutils}/bin/cp -r ${built}/test-build/* .
                       export PATH=${pkgs.csmith}/bin:${llvm}/bin:${clang}/bin:${pkgs.diffutils}/bin:$PATH
                       set -e
                       echo Running unit-test
                       ./dist/build/unit-test/unit-test
                       echo Running disasm-test
                       ./dist/build/disasm-test/disasm-test
                       echo Running fuzzing
                       ./dist/build/fuzz-llvm-disasm/fuzz-llvm-disasm --csmith-path ${pkgs.csmith}/include/csmith* --disasm ./dist/build/llvm-disasm/llvm-disasm
                       echo Finished testing
                       echo OK > $out
                       ''
                     ];
              buildInputs = [ clang llvm pkgs.diffutils pkgs.coreutils ];
              hardeningDisable = [ "all" ];  # tests will build with -O0
            };

        in rec {
          default = llvm-pretty-bc-parser;
          TESTS = wrap "llvm-pretty-bc-parser-TESTS"
            (builtins.map
              (llvm-pretty-bc-parser-test llvm-pretty-bc-parser)
              [
                # NOTE: this is the main location which determines what LLVM
                # versions are tested.  The default is to run each of the listed
                # LLVM versions here in parallel.
                #
                # When adding a new LLVM version, it must be found in a nixpkgs
                # version.  The main nixpkgs used here is nixpkgs-unstable, so a
                # "$ nix flake lock update nixpkgs" should be sufficient to get
                # newer LLVM versions.  However, nixpkgs-unstable tends to drop
                # older versions, so the above might result in an older version
                # not being found.  To fix this, add a new nixpkgs input at the
                # top of this file with a nixpkgs release tag that includes the
                # desired version, and then add that nixpkgs to the 'nixpkg_list'
                # above.

                "10"
                "11"
                "12"
                "13"
                "14"
                "15"
                "16"
                "17"
                "18"
                "19"
              ]
            );
          llvm-pretty = mkHaskell "llvm-pretty" llvm-pretty-src {};
          llvm-pretty-bc-parser = mkHaskell "llvm-pretty-bc-parser" self {
            inherit llvm-pretty tasty-sugar;
            configFlags = ["-ffuzz"];
            # Build the tests, but do not run them.  The tests (specifically the
            # disasm-test) require the presence of llvm and optionally clang.
            # There are multiple versions of llvm and clang that will be used for
            # testing elsewhere, so here just ensure that the tests are saved in
            # an accessible location for the subsequent testing.
            adjustDrv = args: drv:
              with pkgs.haskell.lib;
              drv.overrideAttrs (a:
                { checkPhase = "echo skipping checks";
                  postInstall = ''
                    mkdir $out/test-build
                    cp llvm-pretty-bc-parser.cabal $out/test-build
                    mkdir $out/test-build/disasm-test
                    cp -r disasm-test/tests $out/test-build/disasm-test
                    cp -r disasm-test/known_bugs $out/test-build/disasm-test
                    cp -r dist $out/test-build/
                    '';
                });
          };
        });
    };
}
