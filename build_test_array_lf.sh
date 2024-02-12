#!/bin/bash

set -ex

git clean -fdx

FC=lfortran cmake . -DTEST_DRIVE_BUILD_TESTING=OFF
make -j8
ctest
