#!/bin/sh
make -C algorithms/acotspqap/ acotsp
pushd instances/tsp/tsp-rue-2000/
if [ ! -d ./train ]; then
    ./get.sh
fi
popd
