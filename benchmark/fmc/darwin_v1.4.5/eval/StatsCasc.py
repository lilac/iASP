#!/usr/bin/python

import sys
import os
import time


SUMMARY_SUFFIX = "-summary"


if (len (sys.argv) == 1):
    print ("USAGE: StatsCasc.py casc_version <ProblemSet1> <ProblemSet2> <...>")
    print
    print ("Example: 'StatsCasc.py 19 eps'")
    
    sys.exit (0)
    
elif (len(sys.argv) == 2):
    casc_version = sys.argv[1]
    suites = ["eps", "ept", "heq", "hne", "neq", "nne", "peq", "seq", "sne", "ueq"]

else:
    casc_version = sys.argv[1]
    suites = sys.argv[2:]


problem_files = map (lambda suite: "casc" + casc_version + "_" + suite, suites)
problem_files = filter (lambda file_name: os.path.exists (file_name + SUMMARY_SUFFIX), problem_files)

date_suffix = "." + time.strftime ("%m-%d")
file_name = "casc" + casc_version + SUMMARY_SUFFIX + date_suffix
command = "./StatsProblems.py " + " ".join (problem_files) + " > " + file_name

print ("Writting summary to " + file_name + "...\n")

os.system (command)
os.system ("cat " + file_name)
os.system ("echo '' >> " + file_name)
for problem_file in problem_files:
    os.system ("cat " + problem_file + SUMMARY_SUFFIX + " >> " + file_name)
