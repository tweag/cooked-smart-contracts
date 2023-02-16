{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    nixpkgs-22_11.url = "github:NixOS/nixpkgs/22.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
    ## See https://input-output-hk.github.io/cardano-haskell-packages/
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = inputs@{ flake-parts, CHaP, haskellNix, ... }:

    flake-parts.lib.mkFlake { inherit inputs; } {

      flake.internal-lib = import ./internal-lib.nix;

      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];

      imports = [ crowdfunding/crowdfunding.nix split/split.nix ];

      perSystem = { pkgs, ... }: { formatter = pkgs.nixfmt; };
    };

}
