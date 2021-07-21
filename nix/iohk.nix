# Import the IOHK haskell.nix repo defined
# in the niv sources.
let
  sources = import ./sources.nix { };
in
import sources."haskell.nix"
