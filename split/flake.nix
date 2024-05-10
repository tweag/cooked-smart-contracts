{
  ## We pin a specific version because there is a known bug where GHC96X
  ## and HLS 2.6.0.0 do not work together, while nix groups them.
  ## The issue is described here, and a fix is most likely happening soon.
  ## https://github.com/haskell/haskell-language-server/issues/4046
  inputs.nixpkgs.url =
    "github:NixOS/nixpkgs/63143ac2c9186be6d9da6035fa22620018c85932";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hpkgs = pkgs.haskell.packages.ghc963;
      in {
        formatter = pkgs.nixfmt;

        devShells = let
          ## The minimal dependency set to build the project with `cabal`.
          buildInputs = (with hpkgs; [ ghc cabal-install ]) ++ (with pkgs; [
            libsodium
            secp256k1
            pkg-config
            zlib
            xz
            glibcLocales
            postgresql # For pg_config
            ## We change the way 'blst' is built so that it takes into
            ## account the current architecture of the processor. This
            ## is due to a bug where older processors (>= 10 years)
            ## would not be supported. This should not change anything
            ## on newer machines. This could be revised in the future.
            (blst.overrideAttrs (_: _: {
              buildPhase = ''
                runHook preBuild
                ./build.sh -shared -D__BLST_PORTABLE__ ${
                  lib.optionalString stdenv.hostPlatform.isWindows
                  "flavour=mingw64"
                }
                runHook postBuild
              '';
            }))
          ]);

          ## Needed by `pirouette-plutusir` and `cooked`
          LD_LIBRARY_PATH = with pkgs;
            lib.strings.makeLibraryPath [
              libsodium
              zlib
              xz
              postgresql # For cardano-node-emulator
              openldap # For freer-extras‽
            ];
          LANG = "C.UTF-8";
        in {
          default = pkgs.mkShell {
            ## NOTE: `pkgs.ormolu` must appear before `hpkgs.haskell-language-server`
            ## in the `buildInputs`, so as to take precedence. This ensures that the
            ## version of Ormolu available in the path is that of nixpkgs and not the
            ## one pinned by HLS.
            buildInputs = buildInputs ++ (with pkgs; [ hpack hlint ])
              ++ (with hpkgs; [ ormolu haskell-language-server ]);

            inherit LD_LIBRARY_PATH;
            inherit LANG;
          };
        };

      });

  nixConfig = {
    extra-trusted-substituters = [
      "https://tweag-cooked-validators.cachix.org/"
      "https://pre-commit-hooks.cachix.org/"
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "tweag-cooked-validators.cachix.org-1:g1TP7YtXjkBGXP/VbSTGBOGONSzdfzYwNJM27bn8pik="
      "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
    accept-flake-config = true;
  };
}
