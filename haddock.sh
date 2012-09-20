#!/bin/sh

set -x

rm -Rf dist/doc

cabal haddock -v --hyperlink-source $@
