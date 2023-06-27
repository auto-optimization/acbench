#!/bin/sh
make -C algorithms/acotspqap/ acoqap
pushd instances/qap
if [ ! -d ./RS ]; then
    ./get.sh
fi
popd
