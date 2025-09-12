{
  description = "Description for the project";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    haskell-flake.url = "github:srid/haskell-flake";
    hackage-security = {
      url = "github:bglgwyng/hackage-security?ref=export-rebuildTarIndex";
      flake = false;
    };
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }: {
        haskellProjects.default = {
          projectRoot = pkgs.lib.fileset.toSource {
            root = ./.;
            fileset = pkgs.lib.fileset.unions [
              ./cabal.project
              ./nix-x-cabal-utils.cabal
              ./exe
            ];
          };
          packages = {
            hackage-security.source = "${inputs.hackage-security}/hackage-security";
          };
          devShell = {
            tools = hpkgs: {
              inherit (hpkgs) cabal-fmt fourmolu;
            };
          };
        };
      };
    };
}
