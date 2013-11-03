#! /usr/bin/python

import sys
import os
import re
import math

#TPTP_PATH = "/home/alex/TPTP/protein/"
TPTP_PATH = "/mnt/data/TPTP-v3.1.0/TPTP2X/protein/"

SUMMARY_TYPES = [
    "problems",
    "solved",
    "time",
    "memory",
    ]

RESULT_TYPES = [
    "theorem",
    "non_thm",
    "timeout",
    "memory",
    "crash",
    "unsound"
    ]

VALID_RESULT_TYPES = [
    "theorem",
    "non_thm",
    ]


SUMMARY_SUFFIX = ""


# the columns containing the time and memory statistic
# example:
# Problem           Result     CPU  Memory   Close  Assert   Split Subsume Resolve Compact   Prod. D.Bound
TIME_COLUMN = 3
MEMORY_COLUMN = 4


# output table column width
SUITE_COL_WIDTH = 24
DEFAULT_COL_WIDTH = 8


def compare_problems (problem1, problem2):
    (rating1, name1, _) = problem1
    (rating2, name2, _) = problem2

    if rating1 < rating2:
        return -1
    elif rating1 > rating2:
        return 1
    else:
        if name1 < name2:
            return -1
        elif name1 > name2:
            return 1
        else:
            return 0
    
def sort_problems (problems):
    problems.sort (compare_problems)
    
def problem_rating (problem):
    try:
	file = open (TPTP_PATH + problem[0:3] + "/" + problem + ".tme")

        line = file.readline ()
        while (line):
	    # % Rating   : 0.40 v2.6.0, 0.50 v2.5.0
	    match = re.search ("^% Rating   : (?P<value>\d+\.\d+)", line)
	    if match:
		file.close ()
		return (float (match.group ("value")))
            line = file.readline ()
 
	file.close ()
	raise "no rating for " + problem

    except IOError, (error_nr, error_string):
	print ("Couldn't open " + problem + ": " + error_string)

def process (file_names):
    for file_name in file_names:
        try:
            file = open (file_name)
            file_out = open (file_name + "-rating", "w")

            now_only_problems = 0
            problems = []
            
            line = file.readline ()
            while (line):
                elements = line.split ()

                if len (elements) == 0:
                    ()

                elif now_only_problems:
                    problem = elements[0]
                    rating = problem_rating (problem)
                    problems.append ((rating, problem, line))

                else:
                    if (
                        elements[0] == "Problem"
                        and
                        elements[1] == "Result"):
                        now_only_problems = 1
                        print "Rating " + line,
                        file_out.write ("Rating " + line);

                    else:
                        print line,
                        file_out.write (line);

                line = file.readline ()
                    
            file.close ()

            sort_problems (problems)
            for (rating, _, line) in problems:
                print "[" + (str (rating) + "0")[:4] + "] " + line,
                file_out.write ("[" + (str (rating) + "0")[:4] + "] " + line)

            file_out.close ()

        except IOError, (error_nr, error_string):
            print ("Couldn't open " + file_name + ": " + error_string)
        





###
### main
###

if (len (sys.argv) < 2):
    print ("USAGE: RunProblems.py <ProblemSet1> <ProblemSet2> <...>")
    print
    print ("Example: 'RunProblems.py SYN'")
else:
    process (sys.argv[1:])
