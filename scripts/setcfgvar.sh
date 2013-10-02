#!/bin/bash

cat src/core/config.ml | sed 's!^[ ]*let[ ]*'$1'[ ]*=[ ]*\".*\"!let '$1' = \"'"$2"'\"!' > config.ml
mv config.ml src/core/config.ml
