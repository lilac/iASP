#!/bin/bash
PROBLEM=toh
LOG_DIR=~/log
ivASP_LOG_DIR=$LOG_DIR/ivASP/$PROBLEM
iclingo_LOG_DIR=$LOG_DIR/iclingo/$PROBLEM
if [ ! -d "$ivASP_LOG_DIR" ]
then
  mkdir -p "$ivASP_LOG_DIR"
fi
if [ ! -d "$iclingo_LOG_DIR" ]
then
  mkdir -p "$iclingo_LOG_DIR"
fi

for f in instances/*
do
    name=`basename $f`
    timeout 3600 ivasp "$PROBLEM.ilp" $f >"$ivASP_LOG_DIR/$name.txt"
    timeout 3600 iclingo "$PROBLEM.lp" $f >"$iclingo_LOG_DIR/$name.txt"
    echo "$f done."
done

echo "All done."
