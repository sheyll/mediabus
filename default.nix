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
              packages.mediabus.components.tests.tests = {
                # HACK make 'cabal test' work
                # https://github.com/input-output-hk/haskell.nix/issues/231#issuecomment-731699727
                build-tools = [
                  this.hsPkgs.hspec-discover
                ];
                # END OF HACK
                enableExecutableProfiling = withProfiling;
                ghcOptions = if withProfiling then [ "-fprof-auto" ] else [];
              };
              packages.mediabus.components.library = {
                enableLibraryProfiling = withProfiling;
                ghcOptions = if withProfiling then [ "-fprof-auto" ] else [];
              };
            }
          ];
      };
in
this
