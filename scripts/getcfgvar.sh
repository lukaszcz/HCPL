#!/bin/bash

cat src/core/config.ml | sed -n 's/^[ ]*let[ ]*'$1'[ ]*=[ ]*\"\(.*\)\"/\1/p'
