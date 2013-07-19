#!/bin/bash

echo -n "Running vanilla tests"
for t in tests/vanilla/test_*.ipl
do
    ./ipl --vanilla $t > test.out
    diff -q tests/vanilla/`basename $t .ipl`.out test.out
    rm test.out
    echo -n "."
done
echo " DONE"

echo -n "Running vanilla tests with loaded runtime"
for t in tests/vanilla/test_*.ipl
do
    ./ipl -R lib/core.ipl $t > test.out
    diff -q tests/vanilla/`basename $t .ipl`.out test.out
    rm test.out
    echo -n "."
done
echo " DONE"

echo -n "Running main tests"
for t in tests/main/test_*.ipl
do
    ./ipl -R lib/core.ipl -I tests/main $t > test.out
    diff -q tests/main/`basename $t .ipl`.out test.out
    rm test.out
    echo -n "."
done
echo " DONE"

echo -n "Running slow tests (this may take a few minutes)"
for t in tests/slow/test_*.ipl
do
    ./ipl -R lib/core.ipl $t > test.out
    diff -q tests/slow/`basename $t .ipl`.out test.out
    rm test.out
    echo -n "."
done
echo " DONE"
