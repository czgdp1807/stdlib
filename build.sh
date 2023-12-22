#!/bin/bash

set -ex

git clean -dfx

FC=gfortran cmake .
make fortran_stdlib
make example_gamma
./example/specialfunctions_gamma/example_gamma

git clean -dfx

FC=lfortran cmake .
make fortran_stdlib
cp src/*.mod example/specialfunctions_gamma
make example_gamma
./example/specialfunctions_gamma/example_gamma
