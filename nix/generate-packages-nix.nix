{ pkgs, nixXCabalUtils }:

{ package-set }:
pkgs.runCommand "packages-nix" { nativeBuildInputs = [ nixXCabalUtils pkgs.nix ]; } ''
  export HOME="$TMPDIR"
  export LC_ALL=C.UTF-8

  generate-packages-nix \
    --nixpkgs ${pkgs.path} \
    --package-set ${package-set} \
    > $out
''
