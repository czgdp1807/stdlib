#!/bin/bash

set -ex

git clean -fdx

FC=gfortran cmake . -DTEST_DRIVE_BUILD_TESTING=OFF -DBUILD_EXAMPLE=ON
make -j8
ctest
