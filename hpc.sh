#!/bin/bash -ex

cabal configure --ghc-option=-fhpc --enable-tests
rm -f Spec.tix Spec.mix
cabal build
./dist/build/test/test
hpc markup test
hpc report test
