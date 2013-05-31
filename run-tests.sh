#!/bin/bash

echo -n "Running vanilla tests... "
for t in tests/vanilla/*.ipl
do
    ./ipl --vanilla $t > test.out
    diff -q tests/vanilla/`basename $t .ipl`.out test.out
    rm test.out
done
echo "DONE"

echo -n "Running vanilla tests with loaded runtime... "
for t in tests/vanilla/*.ipl
do
    ./ipl -R lib/core.ipl $t > test.out
    diff -q tests/vanilla/`basename $t .ipl`.out test.out
    rm test.out
done
echo "DONE"

echo -n "Running main tests... "
for t in tests/main/*.ipl
do
    ./ipl -R lib/core.ipl $t > test.out
    diff -q tests/main/`basename $t .ipl`.out test.out
    rm test.out
done
echo "DONE"

echo -n "Running slow tests... "
for t in tests/slow/*.ipl
do
    ./ipl -R lib/core.ipl $t > test.out
    diff -q tests/slow/`basename $t .ipl`.out test.out
    rm test.out
done
echo "DONE"
