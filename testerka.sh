#!/bin/bash

stack run examples/test0$1.ins > examples/test0$1.j
java -jar jasmin.jar examples/test0$1.j 
echo "Correct output:"
cat examples/test0$1.output
echo "Your output:"
java C
rm C.class
