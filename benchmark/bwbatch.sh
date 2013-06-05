#!/bin/bash
mkdir -p iv-ASP-log/bw
mkdir -p iclingo-log/bw
for f in bw/*.lp
do
    `which timeout` 3600 ../linux-build/release/bin/bwasp <$f >"iv-ASP-log/$f.txt"
    timeout 600 ~/bin/iclingo modbw2.lp $f >"iclingo-log/$f.txt"
    echo "$f done."
done
