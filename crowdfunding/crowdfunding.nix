{ self, inputs, config, ... }: {

  perSystem = { config, pkgs, system, ... }:
    let
      crowdfunding = self.internal-lib.make-haskell-nix-pkg {
        inherit (inputs) haskellNix CHaP;
        inherit pkgs;
        src = ./.;
        compiler-nix-name = "ghc8107";
      };
      flake = crowdfunding.flake { };
    in {
      packages.crowdfunding = flake.packages."crowdfunding:test:spec";

      packages.crowdfunding-lib =
        flake.packages."crowdfunding:lib:crowdfunding";

      devShells.crowdfunding = crowdfunding.shellFor {
        withHoogle = true;
        tools = { hpack = { inherit (crowdfunding) index-state; }; };
      };
    };
}
