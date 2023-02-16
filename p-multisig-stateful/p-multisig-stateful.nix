{ self, inputs, config, ... }: {

  perSystem = { config, pkgs, system, ... }:
    let
      p-multisig-stateful = self.internal-lib.make-haskell-nix-pkg {
        inherit (inputs) haskellNix CHaP;
        inherit pkgs;
        src = ./.;
        compiler-nix-name = "ghc8107";
      };
      flake = p-multisig-stateful.flake { };
    in {
      packages.p-multisig-stateful = flake.packages."p-multisig-stateful:test:spec";

      packages.p-multisig-stateful-lib = flake.packages."p-multisig-stateful:lib:p-multisig-stateful";

      devShells.p-multisig-stateful = p-multisig-stateful.shellFor {
        withHoogle = true;
        tools = { hpack = { inherit (p-multisig-stateful) index-state; }; };
      };
    };
}
