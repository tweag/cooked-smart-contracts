{ self, inputs, config, ... }: {

  perSystem = { config, pkgs, system, ... }:
    let
      auction = self.internal-lib.make-haskell-nix-pkg {
        inherit (inputs) haskellNix CHaP;
        inherit pkgs;
        src = ./.;
        compiler-nix-name = "ghc8107";
      };
      flake = auction.flake { };
    in {
      packages.auction = flake.packages."auction:test:spec";

      packages.auction-lib = flake.packages."auction:lib:auction";

      devShells.auction = auction.shellFor {
        withHoogle = true;
        tools = { hpack = { inherit (auction) index-state; }; };
      };
    };
}
