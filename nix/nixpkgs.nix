# This is a function to make a (stable) nixpkgs set from
# the typical configuration set usually passed to
# `import <nixpkgs>` to a pinned IOHK haskell nix nixpkgs version.
import (import ./iohk.nix { }).sources.nixpkgs
