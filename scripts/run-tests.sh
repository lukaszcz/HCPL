#!/bin/bash

echo -n "Running vanilla tests"
for t in tests/vanilla/test_*.hcpl
do
    ./hcpl --vanilla $t > test.out
    diff -q tests/vanilla/`basename $t .hcpl`.out test.out
    rm test.out
    echo -n "."
done
echo " DONE"

echo -n "Running vanilla tests with loaded runtime"
for t in tests/vanilla/test_*.hcpl
do
    ./hcpl -R lib/core.hcpl $t > test.out
    diff -q tests/vanilla/`basename $t .hcpl`.out test.out
    rm test.out
    echo -n "."
done
echo " DONE"

echo -n "Running main tests"
for t in tests/main/test_*.hcpl
do
    ./hcpl -R lib/core.hcpl -I tests/main -I lib $t > test.out
    diff -q tests/main/`basename $t .hcpl`.out test.out
    rm test.out
    echo -n "."
done
echo " DONE"

echo -n "Running logic tests"
for t in tests/logic/test_*.hcpl
do
    ./hcpl -R lib/core.hcpl -I lib $t > test.out
    diff -q tests/logic/`basename $t .hcpl`.out test.out
    rm test.out
    echo -n "."
done
echo " DONE"

echo -n "Running examples"
for t in examples/example_*.hcpl
do
    ./hcpl -R lib/core.hcpl -I lib $t > test.out
    diff -q examples/`basename $t .hcpl`.out test.out
    rm test.out
    echo -n "."
done
echo " DONE"

echo -n "Running slow tests (this may take a few minutes)"
for t in tests/slow/test_*.hcpl
do
    ./hcpl -R lib/core.hcpl $t > test.out
    diff -q tests/slow/`basename $t .hcpl`.out test.out
    rm test.out
    echo -n "."
done
echo " DONE"
