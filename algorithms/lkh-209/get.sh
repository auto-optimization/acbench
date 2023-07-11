#!/bin/sh
wget https://github.com/souzamarcelo/supp-cor-capopt/raw/master/files/src/src-lkh-original.zip
unzip src-lkh-original.zip
mv src-lkh-original/Makefile src-lkh-original/README.txt src-lkh-original/src -t .
rm -rf src-lkh-original src-lkh-original.zip
pushd src/
patch -p1 < ../lkh-209.patch
popd



