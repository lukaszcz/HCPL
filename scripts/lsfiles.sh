#!/bin/bash

find $1 -name '*.ml' -print -o -name '*.mli' -print -o -name '*.mll' -print -o -name '*.ipl' -print | sort
