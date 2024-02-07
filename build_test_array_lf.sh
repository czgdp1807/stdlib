#!/bin/bash

set -ex

git clean -fdx

FC="lfortran --cpp --realloc-lhs" cmake .
make fortran_stdlib
cp src/*.mod test/array
cd test/array
lfortran --cpp --realloc-lhs ../../subprojects/test-drive/src/testdrive.F90 -c
lfortran --realloc-lhs test_logicalloc.f90 -o test_logicalloc
ctest
