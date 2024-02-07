#!/bin/bash

set -ex

git clean -fdx

FC=gfortran cmake . -DTEST_DRIVE_BUILD_TESTING=OFF
make -j8
ctest
