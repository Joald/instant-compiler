#!/bin/bash

set -e
runTest() {
    pref=$1
    i=$2
    printf "Test %d:" $i
    rm -rf examples/$pref$i.ll
    stack run llvm examples/$pref$i.ins > examples/$pref$i.ll
    llvm-as -o examples/$pref$i.bc examples/$pref$i.ll
    llvm-link -o out.bc examples/$pref$i.bc lib/runtime.bc
    lli out.bc >out.out
    diff out.out "examples/$pref$i.output"
    printf "OK\n"
    rm out.bc out.out
}

for i in $(seq -f %02g 1 7); do
    runTest test $i
done

for i in $(seq 0 49); do
    runTest gen $i
done
