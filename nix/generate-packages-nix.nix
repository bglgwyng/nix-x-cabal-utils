{ pkgs, nixXCabalUtils }:

{ package-set }:
pkgs.runCommand "packages-nix" { nativeBuildInputs = [ nixXCabalUtils pkgs.nix ]; } ''
  generate-packages-nix \
    --nixpkgs ${pkgs.path} \
    --package-set ${package-set} \
    > $out
''
