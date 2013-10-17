#!/bin/bash

echo "In the following questions, press ENTER to choose the default."
echo "The default choice is indicated in square brackets."
echo ""

echo -n "Data directory [/usr/share/hcpl]: "
read -e data_dir

echo -n "Binary directory [/usr/bin]: "
read -e bin_dir

if [ -z "$data_dir" ]; then
   data_dir="/usr/share/hcpl"
fi

if [ -z "$bin_dir" ]; then
   bin_dir="/usr/bin"
fi

data_dir=`eval "echo $data_dir"`
bin_dir=`eval "echo $bin_dir"`

scripts/setcfgvar.sh data_dir "$data_dir"
scripts/setcfgvar.sh bin_dir "$bin_dir"

echo "#!/bin/bash" > uninstall.sh
echo "rm -rf $data_dir" >> uninstall.sh
echo "rm -f $bin_dir/hcpl" >> uninstall.sh
echo "rm -f $bin_dir/uninstall-hcpl" >> uninstall.sh

chmod a+x uninstall.sh
