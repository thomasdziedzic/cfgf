#!/bin/bash

cd ./tmp

for pkg in *
do
  echo "Staging ${pkg}"
  cd "${pkg}/trunk"

  stagingpkg 'rebuild with cfgf'

  cd ../..
done
