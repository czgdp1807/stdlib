#!/bin/bash

set -ex

git clean -dfx

FC=lfortran cmake .
make example_gamma || true
cp src/*.mod example/specialfunctions_gamma
make example_gamma
./example/specialfunctions_gamma/example_gamma
