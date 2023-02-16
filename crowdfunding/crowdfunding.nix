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
      ## REVIEW Building the package with Nix currently (2023-02-15) fails
      ## on `cardano-addresses`because of
      ## https://github.com/input-output-hk/haskell.nix/issues/767
      packages.crowdfunding = flake.packages."crowdfunding:lib:crowdfunding";

      devShells.crowdfunding = crowdfunding.shellFor {
        withHoogle = true;
        tools = { hpack = { inherit (crowdfunding) index-state; }; };
      };
    };
}
