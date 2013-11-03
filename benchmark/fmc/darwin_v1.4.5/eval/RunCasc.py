#!/usr/bin/python

import sys
import os

if (len (sys.argv) == 1):
    print ("USAGE: RunCasc.py casc_version <ProblemSet1> <ProblemSet2> <...>")
    print
    print ("Example: 'RunCasc.py 19 eps'")
    
    sys.exit (0)
    
elif (len(sys.argv) == 2):
    casc_version = sys.argv[1]
    suites = ["eps", "ept", "heq", "hne", "neq", "nne", "peq", "seq", "sne", "ueq"]

else:
    casc_version = sys.argv[1]
    suites = sys.argv[2:]

problem_files = map (lambda suite: "casc" + casc_version + "_" + suite, suites)

print ["./RunProblems.py"] + problem_files
os.execvp ("./RunProblems.py", ["./RunProblems.py"] + problem_files)
