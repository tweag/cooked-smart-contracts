# # This file provides miscellaneous Nix utilities
{
  ## Generate a haskell package using haskell.nix. The package is
  ## parameterised by its sources and a compiler version
  ##
  ## NOTE the function uses an overlay, but we're only interested in
  ## one package of the new set of packages.
  make-haskell-nix-pkg = { pkgs, src, haskellNix, CHaP, compiler-nix-name }:
    (pkgs.appendOverlays [
      haskellNix.overlay
      (final: prev: {
        thePackage = final.haskell-nix.project {
          inherit src compiler-nix-name;

          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
          };
        };
      })
    ]).thePackage;
}
