# Offchain code for the Lotto

This Haskell project contains some offchain code for the Lotto.
The onchain code is [there](../onchain).

## Development workflow

To build (and run) the offchain code,

```console
$ nix develop .#lotto-offchain-cabal
$ cabal build
$ cabal run
```

The command `nix develop .#lotto-offchain-cabal` also provides
development oriented tools (_e.g._ haskell-language-server).

## Miscellaneous notes

The offchain code expects the onchain code to be serialised through
[`Ply`](https://github.com/mlabs-haskell/ply) and located in
`scripts/`.
