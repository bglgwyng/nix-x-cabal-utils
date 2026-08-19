{ pkgs, nixXCabalUtils }:

let
  pkgsLib = pkgs.lib;
in
{ repositories, active-repositories ? null }:
let
  repositoryConfig =
    (pkgsLib.evalModules {
      modules = [
        ./repo-config.nix
        { config.repositories = repositories; }
      ];
    }).config.repositories;
  configuredRepositories = pkgsLib.mapAttrs (
    repositoryName: repository:
    let
      repositoryType = builtins.head (builtins.attrNames repository);
      repositoryConfig = repository.${repositoryType};
    in
    if repositoryType == "remote" && repositoryConfig.secure then
      (builtins.removeAttrs repositoryConfig [ "01-index-tar" "root-json" ])
      // {
        type = "remote";
        "cache-directory" = pkgs.runCommand "${repositoryName}-repo-cache" {
          nativeBuildInputs = [ pkgs.gzip nixXCabalUtils ];
        } ''
          mkdir -p $out
          ln -s ${repositoryConfig."01-index-tar"} $out/01-index.tar
          ln -s ${repositoryConfig.root-json} $out/root.json
          generate-secure-repo-index-cache \
            ${repositoryName} \
            $out
        '';
      }
    else if repositoryType == "local" then
      let
        localSources = pkgsLib.concatLists (pkgsLib.mapAttrsToList (
          packageName: sourceOrVersions:
          let
            sources =
              if builtins.isAttrs sourceOrVersions then
                pkgsLib.mapAttrsToList (
                  version: source: {
                    inherit version;
                    inherit source;
                  }
                ) sourceOrVersions
              else
                [
                  {
                    version = null;
                    source = sourceOrVersions;
                  }
                ];
          in
          map (source: source // { inherit packageName; }) sources
        ) repositoryConfig);
        localSource = toString (pkgs.runCommand "${repositoryName}-local-repo" {
          nativeBuildInputs = [ pkgs.gnutar pkgs.gzip ];
        } ''
          mkdir -p $out
          ${pkgsLib.concatStringsSep "\n" (map ({ packageName, source, version }: ''
            version=$(sed -n 's/^version:[[:space:]]*//p' ${source}/${packageName}.cabal)
            ${pkgsLib.optionalString (version != null) ''
              test "$version" = "${version}" || {
                echo "${packageName}: expected version ${version}, found $version" >&2
                exit 1
              }
            ''}
            mkdir -p "$out/${packageName}"
            cp -R ${source}/. "$out/${packageName}/"
            mkdir -p "$TMPDIR/${packageName}-$version"
            cp -R ${source}/. "$TMPDIR/${packageName}-$version/"
            tar -czf "$out/${packageName}-$version.tar.gz" -C "$TMPDIR" "${packageName}-$version"
          '') localSources)}
        '');
      in
      {
        type = "local";
        url = localSource;
        "cache-directory" = pkgs.runCommand "${repositoryName}-repo-cache" {
          nativeBuildInputs = [ nixXCabalUtils ];
        } ''
          mkdir -p $out
          generate-no-index-cache ${localSource} $out
        '';
      }
    else
      throw "unsupported repository type: ${repositoryType}: ${repositoryName}"
  ) repositoryConfig;
in
pkgs.writeText "repos.config.json" (builtins.toJSON (
  { repositories = configuredRepositories; }
  // pkgsLib.optionalAttrs (active-repositories != null) {
    "active-repositories" = active-repositories;
  }
))
