# This file contains a ready-to-use 'nixpkgs' enriched
# with IOHKs haskell eco system.
let
  haskellNix = import ./iohk.nix { };
in
haskellNix.pkgs
