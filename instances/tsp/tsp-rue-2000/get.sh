#!/bin/sh
wget https://iridia.ulb.ac.be/irace/files/tsp-instances-training.tar.bz2
wget https://iridia.ulb.ac.be/irace/files/tsp-instances-testing.tar.bz2
mkdir -p ./train && tar -axvf tsp-instances-training.tar.bz2 --directory ./train
mkdir -p ./test && tar -axvf tsp-instances-testing.tar.bz2 --directory ./test
rm -f tsp-instances-training.tar.bz2 tsp-instances-testing.tar.bz2 
