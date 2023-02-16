{ self, inputs, config, ... }: {

  perSystem = { config, pkgs, system, ... }:
    let
      split = self.internal-lib.make-haskell-nix-pkg {
        inherit (inputs) haskellNix CHaP;
        inherit pkgs;
        src = ./.;
        compiler-nix-name = "ghc8107";
      };
      flake = split.flake { };
    in {
      packages.split-lib = flake.packages."split-contract:lib:split-contract";

      packages.split = flake.packages."split-contract:test:spec";

      devShells.split = split.shellFor {
        withHoogle = true;
        tools = { hpack = { inherit (split) index-state; }; };
        buildInputs = [ pkgs.glibcLocales ];
        LANG = "C.UTF-8";
      };

    };
}
