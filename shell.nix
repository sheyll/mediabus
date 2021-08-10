{ pkgs ? (import ./nix/pkgs.nix) }:
let
  this = pkgs.callPackage ./default.nix { };
in
this.shellFor {
  packages = ps: [ ps.mediabus ];
  buildInputs = [ pkgs.sox ];
  # HACK
  # make hspec-discover available
  # https://github.com/input-output-hk/haskell.nix/issues/231#issuecomment-601469249
  exactDeps = false;
  # END OF HACK

  withHoogle = true;
  tools = {
    cabal = "latest";
    ormolu = "latest";
    haskell-language-server = "latest";
    hlint = "latest";
  };
}
