#!/bin/bash
PROBLEM=gc
LOG_DIR=~/log
ivASP_LOG_DIR=$LOG_DIR/ivASP/$PROBLEM
iclingo_LOG_DIR=$LOG_DIR/iclingo/$PROBLEM

for f in simple-instances/*
do
    name=`basename $f`
    echo '-------------------------------------'
    echo $name$
    echo '#####################################'
    tail -n 6 "$ivASP_LOG_DIR/$name.txt"
    echo '+++++++++++++++++'
    tail -n 6 "$iclingo_LOG_DIR/$name.txt"
done

echo '-------------------------------------'
