#!/bin/bash
mkdir -p iASP-log/gc-s
mkdir -p iclingo-log/gc-s
for f in gc-s/*.lp
do
    `which timeout` 600 ../build/release/bin/gcasp <$f >"iASP-log/$f.txt"
    timeout 600 ~/bin/iclingo modgc.lp $f >"iclingo-log/$f.txt"
    echo "$f done."
done
