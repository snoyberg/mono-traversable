#!/bin/bash -ex

rm -f test.tix test.mix
cabal configure --ghc-option=-fhpc --enable-tests
cabal build
./dist/build/test/test
hpc markup test
hpc report test
