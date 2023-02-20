# Plutarch lotto

This directory provides a Haskell project that implements a lotto on
the Cardano blockchain.
The onchain code is written with
[Plutarch](https://github.com/Plutonomicon/plutarch-plutus)
in the directory `onchain`.
The offchain code is written in
[Plutus](https://plutus.readthedocs.io/en/latest/).

To run the lotto, you need the [Nix](https://nixos.org) package manager.
Then to run the test suite of the lotto,
```command
$ cd onchain
$ nix run .#lotto-onchain -- --ply
$ cd ../offchain
$ nix develop .#lotto-offchain
$ LANG=C.UTF-8 cabal run
```
