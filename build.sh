#!/bin/bash

set -ex

git clean -dfx

FC=gfortran cmake .
make fortran_stdlib
make
./example/specialfunctions_gamma/example_gamma
./example/specialfunctions_gamma/example_gamma_p
./example/specialfunctions_gamma/example_gamma_q
./example/specialfunctions_gamma/example_ligamma
./example/specialfunctions_gamma/example_log_factorial
./example/specialfunctions_gamma/example_log_gamma
./example/specialfunctions_gamma/example_uigamma

git clean -dfx

FC=lfortran cmake .
make fortran_stdlib
cp src/*.mod example/specialfunctions_gamma
make
./example/specialfunctions_gamma/example_gamma
./example/specialfunctions_gamma/example_gamma_p
./example/specialfunctions_gamma/example_gamma_q
./example/specialfunctions_gamma/example_ligamma
./example/specialfunctions_gamma/example_log_factorial
./example/specialfunctions_gamma/example_log_gamma
./example/specialfunctions_gamma/example_uigamma
