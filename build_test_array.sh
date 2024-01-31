#!/bin/bash

set -ex

git clean -fdx

FC=lfortran cmake .
make fortran_stdlib
cp src/*.mod test/array
cd test/array
lfortran ../../subprojects/test-drive/src/testdrive.F90 -c
lfortran test_logicalloc.f90 -o test_logicalloc.out
