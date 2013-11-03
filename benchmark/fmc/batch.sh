#!/bin/bash
PROBLEM=fmc
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

cat instances.txt | \
while read line
do
    read -a words <<< "$line"
    f="${words[1]}.p"
    name=${words[0]}
    ./darwin -fd true -pfdp Exit $f | ./fmc2ivasp.py | cat "$PROBLEM.ilp" - | timeout 300 ivasp >"$ivASP_LOG_DIR/$name.txt"
    ./darwin -fd true -pfdp Exit $f | ./fmc2iasp.py | cat "$PROBLEM.lp" - | timeout 300 iclingo >"$iclingo_LOG_DIR/$name.txt"
    echo "$f done."
done

echo "All done."
