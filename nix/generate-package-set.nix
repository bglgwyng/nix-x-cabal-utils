{ pkgs, nixXCabalUtils }:

{ ghc, ghc-pkg, packages-config, repos-config }:
let
  packagesConfigFile = pkgs.writeText "packages.config.json" (builtins.toJSON packages-config);
  reposConfigFile =
    if builtins.isAttrs repos-config && !(builtins.hasAttr "outPath" repos-config) then
      pkgs.writeText "repos.config.json" (builtins.toJSON { repositories = repos-config; })
    else
      repos-config;
in
pkgs.runCommand "package-set" { nativeBuildInputs = [ nixXCabalUtils ]; } ''
  generate-package-set \
    --ghc ${ghc} \
    --ghc-pkg ${ghc-pkg} \
    --packages-config ${packagesConfigFile} \
    --repos-config ${reposConfigFile} \
    > $out
''
