#!/usr/bin/env sh

## If you want to build a project with `haskell.nix`, you'll have to provide
## checksums for all `source-repository-package` stanzas in the file
## `cabal.project`. This script helps you doing that.
##
## To use it, you need
## - `jq`,
## - `nix-prefetch-git` and
## - `xclip`
##
## USAGE: cp-sha256 REPO USER REF
## to copy the sha256 checksum of the repository
## https://github.com/REPO/USER at commit REF into the clipboard
## using `xclip`.

user="${1:?'User not given'}"
repo="${2:?'Repository not given'}"
ref="${3:?'Reference not given'}"

nix-prefetch-git --quiet https://github.com/"$user"/"$repo" "$ref" \
	| jq -r '.sha256' \
	| xclip
