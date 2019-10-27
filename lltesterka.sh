#!/bin/bash

set -e

for i in $(seq -f %02g 1 7); do
    rm -rf examples/test$i.j
    stack run llvm examples/test$i.ins > examples/test$i.ll
    llvm-as -o examples/test$i.bc examples/test$i.ll
    llvm-link -o out.bc examples/test$i.bc ../llvm/runtime.bc
    echo "Correct output:"
    cat examples/test$i.output
    echo "Your output:"
    lli out.bc
    rm out.bc
done
