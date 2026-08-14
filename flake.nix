{
  description = "Description for the project";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    haskell-flake.url = "github:bglgwyng/haskell-flake?ref=feat/devshell-packages";
    hackage-security = {
      url = "github:bglgwyng/hackage-security?ref=export-rebuildTarIndex";
      flake = false;
    };
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      perSystem =
        {
          config,
          self',
          inputs',
          pkgs,
          system,
          ...
        }:
        let
          inherit (pkgs) lib;
        in
        {
          haskellProjects.default = {
            basePackages = pkgs.haskellPackages;
            packages = {
              hackage-security.source = "${inputs.hackage-security}/hackage-security";

            };
            otherOverlays = [
              (hfinal: hprev: {
                Cabal = hfinal.callHackage "Cabal" "3.16.1.0" { };
                Cabal-syntax = hfinal.callHackage "Cabal-syntax" "3.16.1.0" { };
                Cabal-described = hfinal.callHackage "Cabal-described" "3.16.1.0" { };
                Cabal-QuickCheck = hfinal.callHackage "Cabal-QuickCheck" "3.16.1.0" { };
                Cabal-tests = hfinal.callHackage "Cabal-tests" "3.16.1.0" { };
                # cabal-install's test suite expects this Cabal-internal
                # package, although the resolver library itself does not.
                Cabal-tree-diff = hfinal.callHackage "tree-diff" "0.3.4" { };
                cabal-install-solver = hfinal.callHackage "cabal-install-solver" "3.16.1.0" { };
                cabal-install = hfinal.callHackage "cabal-install" "3.16.1.0" { };
              })
            ];
            projectRoot = lib.fileset.toSource {
              root = ./.;
              fileset = lib.fileset.unions [
                ./cabal.project
                ./nix-x-cabal-utils.cabal
                ./exe
                ./lib
                ./CHANGELOG.md
                ./LICENSE
              ];
            };
            devShell = {
              hlsCheck.enable = false;
              hoogle = false;
              tools = hpkgs: {
                inherit (hpkgs) cabal-gild;
              };
            };
            autoWire = [
              "packages"
              "apps"
              "checks"
            ];
          };
          packages.default = config.haskellProjects.default.outputs.packages.package-set-gen.package;
          packages.hackage-cache =
            let
              hackage-haskell-org_01-index-tar-gz = pkgs.fetchurl {
                url = "https://hackage.haskell.org/01-index.tar.gz";
                hash = "sha256-bRJLiYLXfeXc6tZeCAf8cVEaMSZZ2IwvLdoy/vbUPG8=";
              };
              hackage-haskell-org_root-json = pkgs.fetchurl {
                url = "https://hackage.haskell.org/root.json";
                hash = "sha256-9i5Gy1HUpJmoM2iU16RgcbflKBNa1xYUxxAsHeCu6rw=";
              };
            in
            pkgs.stdenv.mkDerivation {
              name = "ghc914-package-set";
              src = ./data;
              nativeBuildInputs = [
                config.packages.default
              ];
              installPhase = ''
                mkdir -p $out
                cd $out

                ln -s ${hackage-haskell-org_01-index-tar-gz} 01-index.tar.gz
                ln -s ${hackage-haskell-org_root-json} root.json
                gzip -dc 01-index.tar.gz > 01-index.tar
                package-set-gen build-index
              '';
            };
          packages.ghc914-package-set = pkgs.stdenv.mkDerivation {
            name = "ghc914-package-set";
            src = ./data;
            nativeBuildInputs = [
              config.packages.default
            ];
            installPhase = ''
              mkdir -p $out
              package-set-gen build-packagedb \
                --ghc ${pkgs.haskell.compiler.ghc914}/bin/ghc \
                --ghc-pkg ${pkgs.haskell.compiler.ghc914}/bin/ghc-pkg \
                --packages-config ${./data/packages.config.json} \
                --repos-config ${
                  pkgs.writeText "repos.config.json" (
                    builtins.toJSON {
                      repositories = [
                        {
                          "name" = "hackage.haskell.org";
                          "type" = "remote";
                          "url" = "https://hackage.haskell.org/";
                          "secure" = true;
                          "cache-directory" = config.packages.hackage-cache;
                        }
                      ];

                    }
                  )
                } > $out/package-set.json
            '';
          };
          # apps.default = {
          #   type = "app";
          #   program = pkgs.writeShellScriptBin "s" "${config.packages.default}/bin/package-set-gen";
          # };
          devShells.default = pkgs.mkShell {
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell

            ];
            packages = [
              (pkgs.writeShellScriptBin "ghc916" ''
                exec ${pkgs.haskell.compiler.ghc916}/bin/ghc "$@"
              '')
              (pkgs.writeShellScriptBin "ghc-pkg916" ''
                exec ${pkgs.haskell.compiler.ghc916}/bin/ghc-pkg "$@"
              '')
            ];
          };
        };
    };
}
