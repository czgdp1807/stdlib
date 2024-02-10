#!/bin/bash

set -ex

git clean -dfx

FC=gfortran cmake . -DBUILD_EXAMPLE=ON
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

if ./example/error/example_check1 ; then
    echo "Command succeeded. It was expected to fail"
    exit 1
fi
if ./example/error/example_check2 ; then
    echo "Command succeeded. It was expected to fail"
    exit 1
fi
./example/error/example_check3 ;
if ./example/error/example_check4 ; then
    echo "Command succeeded. It was expected to fail"
    exit 1
fi
if ./example/error/example_error_stop1 ; then
    echo "Command succeeded. It was expected to fail"
    exit 1
fi
if ./example/error/example_error_stop2 ; then
    echo "Command succeeded. It was expected to fail"
    exit 1
fi

git clean -dfx

FC="lfortran --cpp" cmake . -DTEST_DRIVE_BUILD_TESTING=OFF -DBUILD_TESTING=OFF -DBUILD_EXAMPLE=ON
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

if ./example/error/example_check1 ; then
    echo "Command succeeded. It was expected to fail"
    exit 1
fi
if ./example/error/example_check2 ; then
    echo "Command succeeded. It was expected to fail"
    exit 1
fi
./example/error/example_check3 ;
if ./example/error/example_check4 ; then
    echo "Command succeeded. It was expected to fail"
    exit 1
fi
if ./example/error/example_error_stop1 ; then
    echo "Command succeeded. It was expected to fail"
    exit 1
fi
if ./example/error/example_error_stop2 ; then
    echo "Command succeeded. It was expected to fail"
    exit 1
fi
