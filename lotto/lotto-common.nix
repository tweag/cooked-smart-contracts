{ self, inputs, ... }: {

  flake.lotto-common = {
    ## Common Haskell development tools, all at the common given `index-state`.
    toolsFor = index-state:
      builtins.mapAttrs (_: value: value // { inherit index-state; }) {
        cabal = { };
        hpack = { };
        haskell-language-server = {
          ## Version 1.9.0.0 requires ghcide 1.9.0.0 which fails to build
          ## (it probably requires GHC 9.x)
          version = "1.8.0.0";
        };
        ormolu = {
          ## Cabal can't compute a proper dependency set with version 0.5
          ## for the offchain code.
          version = "0.4.0.0";
        };
        hlint = {
          ## Cabal can't compute a proper dependency set with version 3.5
          ## for the offchain code.
          version = "3.4";
        };
      };
  };
}
