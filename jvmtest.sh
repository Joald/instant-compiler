#!/bin/bash

set -e

for i in $(seq 1 7); do
    rm -f examples/test0$i.j
    stack run jvm examples/test0$i.ins > examples/test0$i.j
    java -jar jasmin.jar examples/test0$i.j 
    echo "Correct output:"
    cat examples/test0$i.output
    echo "Your output:"
    java C
    rm C.class
done
