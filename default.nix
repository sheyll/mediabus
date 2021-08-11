{ haskell-nix ? (import ./nix/pkgs.nix).haskell-nix
, withProfiling ? false
}:
let
  this =
    haskell-nix.project
      {
        src = haskell-nix.cleanSourceHaskell {
          src = ./.;
          name = "mediabus";
        };
        projectFileName = "cabal.project";
        compiler-nix-name = "ghc8105";
        pkg-def-extras = [];
        modules =
          [
            {
              # HACK make 'cabal test' work
              # https://github.com/input-output-hk/haskell.nix/issues/231#issuecomment-731699727
              packages.mediabus.components.tests.tests.build-tools = [
                this.hsPkgs.hspec-discover
              ];
              # END OF HACK
              packages.mediabus.allComponent = {
                enableExecutableProfiling = withProfiling;
                enableLibraryProfiling = withProfiling;
              } // (
                if withProfiling then
                  {
                    ghcOptions = "-fprof-auto";
                  }
                else {}
              );
            }
          ];
      };
in
this
