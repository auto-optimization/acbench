#!/bin/sh
pushd algorithms/lkh-209/
if [ ! -d ./src ]; then
    ./get.sh
fi
make
popd
pushd instances/tsp/tsp-rue-2000/
if [ ! -d ./train ]; then
    ./get.sh
fi
popd
