#!/usr/bin/env python
import sys

def main(n): # n the number of blocks
    for i in range(1, n+1):
        print("block(%d)." %i)
    print("init(%d, table)." %n)
    for i in range(1, n):
        print("init(%d, %d)." %(i, i+1))
    print("goal(1, table).")
    for i in range(1, n):
        print("goal(%d, %d)." %(i+1, i))

if __name__ == "__main__":
    if (len(sys.argv) >= 2):
        n = int(sys.argv[1])
        main(n)
