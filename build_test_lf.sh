#!/bin/bash

set -ex

git clean -fdx

FC=lfortran cmake . -DTEST_DRIVE_BUILD_TESTING=OFF -DBUILD_EXAMPLE=ON
make
ctest
