#!/bin/bash
PROBLEM=fmc
LOG_DIR=~/log
ivASP_LOG_DIR=$LOG_DIR/ivASP/$PROBLEM
iclingo_LOG_DIR=$LOG_DIR/iclingo/$PROBLEM

cat instances.txt | \
while read line
do
    read -a words <<< "$line"
    f="${words[1]}.p"
    name=${words[0]}
    echo '-------------------------------------'
    echo $name$
    echo '#####################################'
    tail -n 6 "$ivASP_LOG_DIR/$name.txt"
    echo '+++++++++++++++++'
    tail -n 6 "$iclingo_LOG_DIR/$name.txt"
done

echo '-------------------------------------'
