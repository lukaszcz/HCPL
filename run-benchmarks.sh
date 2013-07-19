#!/bin/bash

if [ -f benchmarks.log ]; then
   rm benchmarks.log
fi

for i in 1 2 3
do
    echo | tee -a benchmarks.log
    echo "*************************************************" | tee -a benchmarks.log
    echo "Iteration $i." | tee -a benchmarks.log
    echo | tee -a benchmarks.log

    echo "-------------------------------------------------" | tee -a benchmarks.log
    echo "Running IPL benchmarks... " | tee -a benchmarks.log
    echo | tee -a benchmarks.log

    for t in tests/slow/test_*.ipl
    do
        echo "Running $t..." | tee -a benchmarks.log
        echo | tee -a benchmarks.log
        /usr/bin/time -v ./ipl -t $t 2>&1 | tee -a benchmarks.log
        echo | tee -a benchmarks.log
    done

done
