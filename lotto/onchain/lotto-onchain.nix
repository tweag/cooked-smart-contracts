{ self, inputs, ... }: {
  perSystem = { self', pkgs, ... }:
    let
      lotto-onchain = self.internal-lib.make-haskell-nix-pkg {
        inherit (inputs) haskellNix CHaP;
        inherit pkgs;
        src = "${self}/lotto/onchain";
        compiler-nix-name = "ghc925";
      };
    in {
      devShells.lotto-onchain = lotto-onchain.shellFor {
        tools = self.lotto-common.toolsFor lotto-onchain.index-state;
      };
      packages.lotto-onchain = lotto-onchain.lotto-onchain.components.exes.lotto-onchain;
      apps.lotto-onchain = {
        type = "app";
        program = "${self'.packages.lotto-onchain}/bin/lotto-onchain";
      };
    };
}
