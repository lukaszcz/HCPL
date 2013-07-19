#!/bin/bash

if [ -f ml-benchmarks.log ]; then
   rm ml-benchmarks.log
fi

for i in 1 2 3
do
    echo | tee -a ml-benchmarks.log
    echo "*************************************************" | tee -a ml-benchmarks.log
    echo "Iteration $i." | tee -a ml-benchmarks.log
    echo | tee -a ml-benchmarks.log

    echo "-------------------------------------------------" | tee -a ml-benchmarks.log
    echo "Running ML benchmarks... " | tee -a ml-benchmarks.log
    echo | tee -a ml-benchmarks.log

    for t in tests/slow/test_*.ml
    do
        echo -n "Compiling $t..."
        ocamlc nums.cma $t
        echo "DONE"
        rm tests/slow/*.cmo
        rm tests/slow/*.cmi
        echo "Running $t..." | tee -a ml-benchmarks.log
        echo | tee -a ml-benchmarks.log
        /usr/bin/time -v ./a.out 2>&1 | tee -a ml-benchmarks.log
        rm a.out
        echo | tee -a ml-benchmarks.log
    done
done
