{ lib, ... }:

{
  options.repositories = lib.mkOption {
    type = lib.types.attrsOf (lib.types.attrTag {
      remote = lib.mkOption {
        type = lib.types.submodule {
          options = {
            url = lib.mkOption { type = lib.types.str; };
            secure = lib.mkOption {
              type = lib.types.bool;
              default = true;
            };
            "01-index-tar" = lib.mkOption { type = lib.types.path; };
            root-json = lib.mkOption { type = lib.types.path; };
          };
        };
      };
      local = lib.mkOption {
        type = lib.types.attrsOf (
          lib.types.oneOf [
            lib.types.path
            (lib.types.attrsOf lib.types.path)
          ]
        );
      };
    });
  };
}
