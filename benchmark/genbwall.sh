#!/bin/bash
lb=1
ub=20
options='l:u:'
while getopts $options opt; do
    case $opt in
        l)
            lb=$OPTARG
            echo "lb=$lb"
            ;;
        u)
            ub=$OPTARG
            echo "ub=$ub"
            ;;
        :)
            echo "$OPTARG requires an argumetn.";;
        \?)
            echo "Invalid option.";;
    esac
done

for ((i=lb; i <= ub; i+=5)) # i in {2..20..1}
do
    ./genbw.py $i >"bw/world$i.lp"
done
