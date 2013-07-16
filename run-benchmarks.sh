#!/bin/bash

rm benchmarks.log

echo "Running ML benchmarks... " | tee -a benchmarks.log
echo | tee -a benchmarks.log

for t in tests/slow/test_*.ml
do
    echo -n "Compiling $t..."
    ocamlc $t
    echo "DONE"
    rm tests/slow/*.cmo
    rm tests/slow/*.cmi
    echo "Running $t..." | tee -a benchmarks.log
    echo | tee -a benchmarks.log
    /usr/bin/time -v ./a.out 2>&1 | tee -a benchmarks.log
    rm a.out
    echo | tee -a benchmarks.log
done

echo "Running IPL benchmarks... " | tee -a benchmarks.log
echo | tee -a benchmarks.log

for t in tests/slow/test_*.ipl
do
    echo "Running $t..." | tee -a benchmarks.log
    echo | tee -a benchmarks.log
    /usr/bin/time -v ./ipl -t $t 2>&1 | tee -a benchmarks.log
    echo | tee -a benchmarks.log
done
