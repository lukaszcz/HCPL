#!/bin/bash

function f () {

rm -r *~ *.bak > /dev/null 2>&1

for DIR in *
do
  if [ -d "$DIR" ]; then
      cd "$DIR"
      f
      cd ..
  fi
done

}

f
