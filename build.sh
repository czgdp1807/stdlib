#!/bin/bash

set -ex

git clean -dfx

FC=lfortran cmake .
make || true
cp src/*.mod example/specialfunctions_gamma
make
ctest
