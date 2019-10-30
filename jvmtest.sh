#!/bin/bash
set -e

runTest() {
    pref=$1
    i=$2
    printf "Test %d:\n" $i
    rm -f examples/$pref$i.j
    stack run jvm examples/$pref$i.ins > examples/$pref$i.j
    java -jar lib/jasmin.jar examples/$pref$i.j
    java $pref$i > out.out
    diff out.out "examples/$pref$i.output"
    if [ $? -ne 0 ]; then
        printf "Test failed!\nCorrect output: %d\nYour output: %d" $correct $yours
    else
        printf "OK\n"
    fi
    rm $pref$i.class out.out
}

for i in $(seq 1 7); do
    runTest test0 $i
done
printf "\n\nNow generated tests...\n\n"
for i in $(seq 0 49); do
    runTest gen $i
done
