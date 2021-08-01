#!/usr/bin/env bash

cabal haddock --docdir=docs --haddock-executables --haddock-internal --haddock-hyperlink-source --haddock-option="--ignore-all-exports"
