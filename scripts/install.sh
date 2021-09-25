#!/bin/bash

data_dir=`scripts/getcfgvar.sh data_dir`
bin_dir=`scripts/getcfgvar.sh bin_dir`

mkdir -p $data_dir
cp -r lib/ $data_dir/lib
cp -r examples/ $data_dir/examples
cp -r tests/ $data_dir/tests
cp README $data_dir/README
mkdir -p $data_dir/bin
cp hcpl $data_dir/bin/hcpl
cp uninstall.sh $data_dir/bin/uninstall.sh

ln -s $data_dir/bin/hcpl $bin_dir/hcpl
ln -s $data_dir/bin/uninstall.sh $bin_dir/uninstall-hcpl

scripts/install-kate-files.sh
