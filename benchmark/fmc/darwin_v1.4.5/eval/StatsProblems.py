#! /usr/bin/python

import sys
import os
import re
import math


SUMMARY_TYPES = [
    "problems",
    "solved",
    "time",
    "memory",
    ]

RESULT_TYPES = [
    "theorem",
    "unsat",
    "cosat",
    "sat",
    "timeout",
    "resout",
    "memory",
    "crash",
    "unsound"
    ]

VALID_RESULT_TYPES = [
    "theorem",
    "non_thm",
    "unsat",
    "cosat",
    ]


SUMMARY_SUFFIX = ""
#SUMMARY_SUFFIX = "-summary"


# the columns containing the time and memory statistic
# example:
# Problem           Result     CPU  Memory   Close  Assert   Split Subsume Resolve Compact   Prod. D.Bound
TIME_COLUMN = 3
MEMORY_COLUMN = 4


# output table column width
SUITE_COL_WIDTH = 30
DEFAULT_COL_WIDTH = 8


def print_header ():
    print ("suite".ljust (SUITE_COL_WIDTH)),

    for entry in SUMMARY_TYPES:
        print (entry.rjust (DEFAULT_COL_WIDTH)),

    for entry in RESULT_TYPES:
        print (entry.rjust (DEFAULT_COL_WIDTH)),

    print




def print_summary (suite):
    summary = {}
    results = {}

    for key in SUMMARY_TYPES:
        summary[key] = 0

    for key in RESULT_TYPES:
        results[key] = 0

    file_name = suite + SUMMARY_SUFFIX

    # go through the summary
    try:
        file = open (file_name)

        for line in file.readlines ():
            for key in RESULT_TYPES:
                if re.search (key, line):
                    summary["problems"] = summary["problems"] + 1
                    results[key] = results[key] + 1

                    if key in VALID_RESULT_TYPES:
                        elements = line.split ()
                        summary["time"] = summary["time"] + float (elements[TIME_COLUMN - 1])
                        summary["memory"] = summary["memory"] + float (elements[MEMORY_COLUMN - 1])
                    break

    except IOError, (error_nr, error_string):
        print ("Couldn't open " + file_name + ": " + error_string)

    summary["solved"] = results["theorem"] + results["unsat"] + results["sat"] + results["cosat"]
    if summary["solved"] <> 0:
        summary["time"] = (math.floor ((summary["time"] / summary["solved"]) * 10 + 0.5)) / 10
        summary["memory"] = (math.floor ((summary["memory"] / summary["solved"]) * 10 + 0.5)) / 10

    # print the summary
    print suite.ljust (SUITE_COL_WIDTH),
    for entry in SUMMARY_TYPES:
        print str (summary[entry]).rjust (DEFAULT_COL_WIDTH),

    for entry in RESULT_TYPES:
        print str (results[entry]).rjust (DEFAULT_COL_WIDTH),

    print


    
if (len (sys.argv) == 1):
    print ("USAGE: StatsProblems.py <ProblemSet1> <ProblemSet2> <...>")
    print
    print ("Example: 'StatsProblems.py SYN-summary'")

    sys.exit (0)

else:
    suites = sys.argv[1:]

    print_header ()
    for suite in suites:
        print_summary (suite)
