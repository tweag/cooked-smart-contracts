## This file provides miscellaneous Nix utilities
{
  ## Generate a haskell package using haskell.nix
  make-haskell-nix-pkg =
    { pkgs, src, haskellNix, CHaP, compiler-nix-name, shell }:
    (pkgs.appendOverlays [
      haskellNix.overlay
      (final: prev: {
        thePackage = final.haskell-nix.project {
          inherit src compiler-nix-name shell;

          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP;
          };
        };
      })
    ]).thePackage;
}
