#!/bin/bash

stack run llvm examples/test0$1.ins > examples/test0$1.ll
llvm-as -o examples/test0$1.bc examples/test0$1.ll
llvm-link -o out.bc examples/test0$1.bc ../llvm/runtime.bc
echo "Correct output:"
cat examples/test0$1.output
echo "Your output:"
lli out.bc
rm out.bc
