{
  # nix build .
  # nix develop
  # nix run .  [runs llvm-disasm]
  #
  # nix run github:galoisinc/llvm-pretty-bc-parser   [runs llvm-disasm]

  description = "Flake to build the haskell-src package 'llvm-pretty-bc-parser' and dependencies";

  nixConfig.bash-prompt-suffix = "llvm-pretty-bc-parser.env} ";

  inputs = {
    nixpkgs = { url = "github:nixos/nixpkgs/23.05"; };
    levers = {
      url = "github:kquick/nix-levers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    llvm-pretty-src = {
      url = "github:elliottt/llvm-pretty";
      flake = false;
    };
    fgl-src = {
      url = "github:haskell/fgl/5.8.1.1";
      flake = false;
    };
    fgl-visualize-src = {
      url = "https://hackage.haskell.org/package/fgl-visualize-0.1.0.1/fgl-visualize-0.1.0.1.tar.gz";
      flake = false;
    };
    optparse-applicative-src = {
      url = "github:pcapriotti/optparse-applicative/0.18.1";
      flake = false;
    };
    tasty-src = {
      url = "github:UnkindPartition/tasty/core-1.4.3";
      flake = false;
    };
    tasty-expected-failure-src = {
      url = "github:nomeata/tasty-expected-failure";
      flake = false;
    };
    tasty-sugar = {
      url = "github:kquick/tasty-sugar";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.levers.follows = "levers";
      inputs.optparse-applicative-src.follows = "optparse-applicative-src";
      inputs.tasty-src.follows = "tasty-src";
    };
  };

  outputs = { self, levers, nixpkgs
            , llvm-pretty-src
            , fgl-src
            , fgl-visualize-src
            , optparse-applicative-src
            , tasty-src
            , tasty-expected-failure-src
            , tasty-sugar
            }:
    let
      shellWith = pkgs: adds: drv: drv.overrideAttrs(old:
        { buildInputs = old.buildInputs ++ adds pkgs; });
      # Add additional packages useful for a development shell, generally
      # representing test packages or non-propagated build dependencies of
      # various sub-packages.
      shellPkgs = pkgs: [
        # pkgs.haskell.compiler.integer-simple.ghc8107
        # pkgs.haskell.packages.ghc8107.profiteur
        pkgs.cabal-install
        pkgs.llvm_11
        pkgs.clang_11
      ];
    in rec {
      devShells =
        let oneshell = s: n:
              let pkgs = import nixpkgs { system=s; };
              in levers.variedTargets
                { ghcver = levers.validGHCVersions pkgs.haskell.compiler; }
                ( { ghcver, ... } @ vargs:
                  shellWith pkgs shellPkgs
                    (self.packages.${s}.${n}.${ghcver}.env.overrideAttrs (a:
                      {
                        # Set envvars here
                      }
                    )));
        in levers.eachSystem
          (s:
            let pkgs = import nixpkgs { system=s; };
                names = builtins.attrNames (self.packages.${s});
                outs = builtins.removeAttrs
                  (pkgs.lib.genAttrs names (oneshell s))
                  [ "ghc" ];
                shells = pkgs.lib.attrsets.mapAttrs (n: v: v.default) outs;
            in shells // { default = devShells.${s}.llvm-pretty-bc-parser-test-build; }
          ) ;

      packages = levers.eachSystem (system:
        let
          mkHaskell = levers.mkHaskellPkg {
            inherit nixpkgs system;
            };
          pkgs = import nixpkgs { inherit system; };
          wrap = levers.pkg_wrapper system pkgs;
          haskellAdj = drv:
            with (pkgs.haskell).lib;
            dontHaddock (dontCheck (dontBenchmark (drv)));
          llvm-pretty-bc-parser-test = built: llvmver:
            let llvm = pkgs."llvm_${llvmver}";
                clang = pkgs."clang_${llvmver}";
            in derivation {
              inherit system;
              name = "llvm-pretty-bc-parser-test-with-llvm${llvmver}";
              builder = "${pkgs.bash}/bin/bash";
              args = [ "-c"
                       ''
                       ${pkgs.coreutils}/bin/cp -r ${built}/test-build/* .
                       export PATH=${llvm}/bin:${clang}/bin:${pkgs.diffutils}/bin:$PATH
                       set -e
                       echo Running unit-test
                       ./dist/build/unit-test/unit-test
                       echo Running disasm-test
                       ./dist/build/disasm-test/disasm-test
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
              (llvm-pretty-bc-parser-test llvm-pretty-bc-parser-test-build)
              [
                # NOTE: this is the main location which determines what LLVM
                # versions are tested.  The default is to run each of the listed
                # LLVM versions here in parallel.
                "10"
                "11"
                "12"
                "13"
                "14"
                "15"
                "16"
              ]
            );
          TESTS_PREP = wrap "llvm-pretty-bc-parser-TESTS_PREP"
            [ llvm-pretty-bc-parser-test-build ];
          DOC = wrap "llvm-pretty-bc-parser-DOC"
            [ llvm-pretty-bc-parser-doc ];
          fgl = mkHaskell "fgl" fgl-src {
            adjustDrv = args: haskellAdj;
          };
          fgl-visualize = mkHaskell "fgl-visualize" fgl-visualize-src {
            inherit fgl;
            adjustDrv = args: haskellAdj;
          };
          llvm-pretty = mkHaskell "llvm-pretty" llvm-pretty-src {
            adjustDrv = args: haskellAdj;
          };
          llvm-pretty-bc-parser = mkHaskell "llvm-pretty-bc-parser" self {
            inherit llvm-pretty fgl fgl-visualize;
            adjustDrv = args: haskellAdj;
          };
          llvm-pretty-bc-parser-test-build = mkHaskell "llvm-pretty-bc-parser-test-build" self {
            inherit llvm-pretty optparse-applicative tasty-sugar
              tasty tasty-quickcheck tasty-expected-failure tasty-hunit;
            adjustDrv = args: drv:
              with pkgs.haskell.lib;
              (doCheck (haskellAdj drv)).overrideAttrs (a:
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
          llvm-pretty-bc-parser-doc = mkHaskell "llvm-pretty-bc-parser-doc" self {
            inherit llvm-pretty;
            adjustDrv = args: drv:
              pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.dontBenchmark drv);
          };
          optparse-applicative = mkHaskell "optparse-applicative" optparse-applicative-src {
            adjustDrv = args: haskellAdj;
          };
          tasty = mkHaskell "tasty" "${tasty-src}/core" {
            inherit optparse-applicative;
            adjustDrv = args: haskellAdj;
          };
          tasty-expected-failure = mkHaskell "tasty-expected-failure" "${tasty-expected-failure-src}" {
            inherit tasty;
            adjustDrv = args: haskellAdj;
          };
          tasty-hunit = mkHaskell "tasty-hunit" "${tasty-src}/hunit" {
            inherit tasty;
            adjustDrv = args: haskellAdj;
          };
          tasty-quickcheck = mkHaskell "tasty-quickcheck" "${tasty-src}/quickcheck" {
            inherit optparse-applicative tasty;
            adjustDrv = args: haskellAdj;
          };
        });
    };
}
