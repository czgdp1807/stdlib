#!/bin/bash

set -ex

git clean -fdx

FC=gfortran cmake .
make fortran_stdlib
gfortran subprojects/test-drive/src/testdrive.F90 -c
gfortran test/array/test_logicalloc.f90 -o test_logicalloc.out
./test_logicalloc.out
