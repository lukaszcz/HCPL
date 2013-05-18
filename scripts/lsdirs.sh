#!/bin/bash

function f
{
    echo $2
    cd $1
    for d in *
    do
        if [ -d $d ]; then
            f $d "$2/$d"
        fi
    done
    cd ..
}

f $1 $1
