#!/bin/bash

grep -n "assert" `scripts/lsfiles.sh src` | grep ":$1"
