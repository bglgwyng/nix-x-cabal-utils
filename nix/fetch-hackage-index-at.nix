{ pkgs, nixXCabalUtils }:

{ repoUrl, indexState, hash }:
pkgs.runCommand "${pkgs.lib.replaceStrings [ ":" ] [ "-" ] "index-${indexState}"}" {
  nativeBuildInputs = [ pkgs.cacert pkgs.curl pkgs.gzip nixXCabalUtils ];
  outputHash = hash;
  outputHashAlgo = "sha256";
  outputHashMode = "flat";
} ''
  curl --fail --location --retry 3 \
    --cacert ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt \
    ${pkgs.lib.escapeShellArg "${pkgs.lib.removeSuffix "/" repoUrl}/01-index.tar.gz"} \
    | gzip -dc > index.tar
  cut-index-tar \
    ${pkgs.lib.escapeShellArg indexState} \
    index.tar \
    $out
''
