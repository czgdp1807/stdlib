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

./example/ascii/example_ascii_reverse
./example/ascii/example_ascii_to_lower
./example/ascii/example_ascii_to_sentence
./example/ascii/example_ascii_to_title
./example/ascii/example_ascii_to_upper

./example/version/example_version

./example/optval/example_optval

git clean -dfx

FC=lfortran cmake .
make fortran_stdlib
cp src/*.mod example/specialfunctions_gamma
cp src/*.mod example/ascii
cp src/*.mod example/version
cp src/*.mod example/optval
make
./example/specialfunctions_gamma/example_gamma
./example/specialfunctions_gamma/example_gamma_p
./example/specialfunctions_gamma/example_gamma_q
./example/specialfunctions_gamma/example_ligamma
./example/specialfunctions_gamma/example_log_factorial
./example/specialfunctions_gamma/example_log_gamma
./example/specialfunctions_gamma/example_uigamma

./example/ascii/example_ascii_reverse
./example/ascii/example_ascii_to_lower
./example/ascii/example_ascii_to_sentence
./example/ascii/example_ascii_to_title
./example/ascii/example_ascii_to_upper

./example/version/example_version

./example/optval/example_optval
