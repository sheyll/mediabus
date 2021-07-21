{ haskell-nix ? (import ./nix/pkgs.nix).haskell-nix
, withProfiling ? false
}:
haskell-nix.project {
  src = haskell-nix.cleanSourceHaskell {
    src = ./.;
    name = "mediabus";
  };
  configureArgs = "";

  projectFileName = "cabal.project";
  compiler-nix-name = "ghc8105";
  pkg-def-extras = [ ];
  modules =
    if withProfiling then
      [
        {
          packages.mediabus.package.ghcOptions = "-fprof-auto";
          packages.mediabus.components.library.enableLibraryProfiling = true;
        }
      ] else [{ }];
}
