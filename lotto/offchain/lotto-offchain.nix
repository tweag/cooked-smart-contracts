{ self, inputs, ... }: {

  perSystem = { self', pkgs, final, inputs', ... }:
    let
      lotto-offchain = self.internal-lib.make-haskell-nix-pkg {
        inherit (inputs) haskellNix CHaP;
        inherit pkgs;
        src = "${self}/lotto/offchain";
        compiler-nix-name = "ghc8107";
      };
    in {
      devShells.lotto-offchain = lotto-offchain.shellFor {
        tools = self.lotto-common.toolsFor lotto-offchain.index-state;
        buildInputs = [
          ## For UTF-8 locales
          pkgs.glibcLocales
        ];
        LANG = "C.UTF-8";
      };
      packages.lotto-offchain =
        lotto-offchain.lotto-offchain.components.exes.lotto-offchain;
    };
}
