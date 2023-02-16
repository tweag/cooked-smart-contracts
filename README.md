# [Cooked smart contracts](https://github.com/tweag/cooked-smart-contracts)

Copyright Tweag I/O 2023

If you want to write and test smart contracts for the
[Cardano](https://cardano.org) blockchain using
[Cooked-validators](https://github.com/tweag/plutus-libs),
you can look at the contracts here.

You are free to copy, modify and distribute `Cooked smart contracts`
with attribution under the terms of the MIT license.
See the [license](./LICENSE) file for details.

## Getting started

The examples are packaged using [Nix](https://nixos.org) flakes.
Therefore, if you have the Nix package manager, you can run the
tests for the crowdfunding contract with

```command
$ cd crowdfunding
$ nix develop .#crowdfunding
$ cabal test all
```

## Contribute

You can report bug and ask questions on the
[issue tracker](https://github.com/tweag/cooked-smart-contracts/issues).
