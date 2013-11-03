#!/usr/bin/env python

import os
import sys

filemap = dict()

def init(d):
  filemap[d] = {}
  path = d + '/' + d + 'Problems'
  with open(path) as f:
    for line in f:
      tokens = line.split()
      name = tokens[0]
      fn = tokens[2]
      filemap[d][name] = fn

def main():
  if len(sys.argv) < 2:
    print "argc < 2."
    exit(1)
  fn = sys.argv[1]
  with open(fn) as f:
    for line in f:
      (name, directory) = line.split()
      if filemap.get(directory) is None:
        init(directory)
      else:
        path = directory + '/' + filemap[directory][name]
        print name + ' ' + path

if __name__ == '__main__':
  main()

