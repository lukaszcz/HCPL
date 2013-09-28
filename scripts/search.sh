#!/bin/bash

grep -n "$1" `scripts/lsfiles.sh src` `scripts/lsfiles.sh lib`
