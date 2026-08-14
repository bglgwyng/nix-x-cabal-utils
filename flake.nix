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
      transposition.lib.adHoc = true;
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
          nixXCabalUtils = config.haskellProjects.default.outputs.packages."nix-x-cabal-utils".package;
          generate-repos-config = import ./nix/generate-repos-config.nix { inherit pkgs nixXCabalUtils; };
          generate-package-set = import ./nix/generate-package-set.nix { inherit pkgs nixXCabalUtils; };
          generate-packages-nix = import ./nix/generate-packages-nix.nix { inherit pkgs nixXCabalUtils; };
          fetch-hackage-index-at = import ./nix/fetch-hackage-index-at.nix { inherit pkgs nixXCabalUtils; };
        in
        {
          lib = {
            inherit
              fetch-hackage-index-at
              generate-package-set
              generate-packages-nix
              generate-repos-config
              ;
          };
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
            projectRoot = pkgs.lib.fileset.toSource {
              root = ./.;
              fileset = pkgs.lib.fileset.unions [
                ./cabal.project
                ./nix-x-cabal-utils.cabal
                ./exe
                ./exe/cut-index-tar.hs
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

          packages.default = nixXCabalUtils;

          devShells.default = pkgs.mkShell {
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
            ];
          };
        };
    };
}
