{
  # nix build .
  # nix develop
  # nix run .  [runs llvm-disasm]
  #
  # nix run github:galoisinc/llvm-pretty   [runs llvm-disasm]

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
  };

  outputs = { self, levers, nixpkgs
            , llvm-pretty-src
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
            in shells
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
          llvm-pretty-bc-parser-test = built: llvm:
            derivation {
              inherit system;
              name = "llvm-pretty-bc-parser-test-with-llvm${llvm.version}";
              builder = "${pkgs.bash}/bin/bash";
              args = [ "-c"
                       ''
                       ${pkgs.coreutils}/bin/cp -r ${built}/test-build/* .
                       export PATH=${llvm}/bin:${pkgs.diffutils}/bin:$PATH
                       ./dist/build/unit-test/unit-test
                       ./dist/build/disasm-test/disasm-test
                       echo OK > $out
                       ''
                     ];
              buildInputs = [ llvm pkgs.diffutils pkgs.coreutils ];
            };
        in rec {
          default = llvm-pretty-bc-parser;
          TESTS = wrap "llvm-pretty-bc-parser-TESTS"
            (builtins.map
              (llvm-pretty-bc-parser-test llvm-pretty-bc-parser-test-build)
              [
                pkgs.llvm_10
                pkgs.llvm_11
              ]
            );
          TESTS_PREP = wrap "llvm-pretty-bc-parser-TESTS_PREP"
            [ llvm-pretty-bc-parser-test-build ];
          DOC = wrap "llvm-pretty-bc-parser-DOC"
            [ llvm-pretty-bc-parser-doc ];
          llvm-pretty = mkHaskell "llvm-pretty" llvm-pretty-src {
            adjustDrv = args: haskellAdj;
          };
          llvm-pretty-bc-parser = mkHaskell "llvm-pretty-bc-parser" self {
            inherit llvm-pretty;
            adjustDrv = args: haskellAdj;
          };
          llvm-pretty-bc-parser-test-build = mkHaskell "llvm-pretty-bc-parser-test-build" self {
            inherit llvm-pretty;
            adjustDrv = args: drv:
              with pkgs.haskell.lib;
              (doCheck (haskellAdj drv)).overrideAttrs (a:
                { checkPhase = "echo skipping checks";
                  postInstall = ''
                    mkdir $out/test-build
                    cp llvm-pretty-bc-parser.cabal $out/test-build
                    mkdir $out/test-build/disasm-test
                    cp -r disasm-test/tests $out/test-build/disasm-test
                    cp -r dist $out/test-build/
                    '';
                });
          };
          llvm-pretty-bc-parser-tests = mkHaskell "llvm-pretty-bc-parser-tests" self {
            inherit llvm-pretty;
            adjustDrv = args: drv:
              with pkgs.haskell.lib;
              addExtraLibrary (dontHaddock drv) pkgs.llvm;
          };
          llvm-pretty-bc-parser-doc = mkHaskell "llvm-pretty-bc-parser-doc" self {
            inherit llvm-pretty;
            adjustDrv = args: drv:
              pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.dontBenchmark drv);
          };
        });
    };
}
