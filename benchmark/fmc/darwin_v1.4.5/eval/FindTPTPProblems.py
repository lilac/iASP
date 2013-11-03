#! /usr/bin/python

# finds TPTP problems (in tme format) which match certain criteria

import os
import re



TPTP_PATH = "/media/shared/Benchmarks/TPTP-v4.0.0/Problems/"
#TPTP_PATH = "/mnt/data/TPTP-v3.0.1/Problems/protein/"
#TPTP_PATH = "/mnt/data/TPTP/"
#TPTP_PATH = "/home/alex/TPTP/protein/"
#TPTP_PATH = "/var/benchmarks/TPTPv2.7.0/"
SUFFIX = ".p"



#
# filter functions
#
# return true for problems matching the criterion

# filter by problem status
def filter_status (problem, status):
    for line in problem:
        match = re.search ("^% Status\s+: (?P<value>(\S)*)\s*$", line)
        if match:
            if match.group ("value") == status:
                return 1
            
            else:
                return 0

def filter_satisfiable (problem):
    # % Status   : satisfiable
    return filter_status (problem, "Satisfiable") or filter_status (problem, "CounterSatisfiable")

def filter_unsatisfiable (problem):
    # % Status   : unsatisfiable
    return filter_status (problem, "Unsatisfiable") or filter_status (problem, "Theorem")

def filter_unknown (problem):
    # % Status   : unknown
    return filter_status (problem, "Unknown")

def filter_open (problem):
    # % Status   : open
    return filter_status (problem, "Open")

# drop non Bernays-Schoenfinkel problems
def filter_bernays_schoenfinkel (problem):
    for line in problem:
        # % Syntax   : Number of clauses    :   20 (   0 non-Horn;   2 unit;  20 RR)
        match = re.search ("^%\s+Maximal term depth\s+:\s+(?P<value>\d+)", line)
        if match:
            if match.group ("value") == "1":
                return 1
            
            else:
                return 0

    return 0

# drop non-Horn problems
def filter_horn (problem):
    for line in problem:
        # % Syntax   : Number of clauses    :   20 (   0 non-Horn;   2 unit;  20 RR)
        match = re.search ("(?P<value>\d+) non-Horn", line)
        if match:
            if match.group ("value") == "0":
                return 1
            
            else:
                return 0

    return 0


# keep only problems without equality
def filter_non_equality (problem):
    for line in problem:
        # %            Number of literals   :   38 (   0 equality)
        match = re.search ("(?P<value>\d+) equality", line)
        if match:
            if match.group ("value") == "0":
                return 1
            
            else:
                return 0

    return 0

# keep only problems with equality
def filter_equality (problem):
    for line in problem:
        # %            Number of literals   :   38 (   0 equality)
        match = re.search ("(?P<value>\d+) equality", line)
        if match:
            if match.group ("value") == "0":
                return 0
            
            else:
                return 1

    return 0


# keep only horn problems
def filter_hne (problem):
    return filter_horn (problem)

# keep only non-horn problems
def filter_nne (problem):
    return (not (filter_horn (problem)))


# invalid specifications
def filter_non_FOF (problem):
    for line in problem:
        match = re.search ("%----FOF format not yet installed in PROTEIN", line)
        if match:
            return 0

    return 1
    
def filter_FOF (problem):
    for line in problem:
        match = re.search ("%----FOF format not yet installed in PROTEIN", line)
        if match:
            return 1

    return 0

# filter by rating
def filter_rating (problem):
# % Rating   : 0.40 v2.6.0, 0.50 v2.5.0
# % Rating   : 1.00 v2.1.0
    for line in problem:
        match = re.search ("^% Rating   : 1.00", line)
        if match:
            return 1

    return 0

# top filter function
def filter (problem):
    return (
        #problem
        #filter_FOF (problem)
        #filter_non_FOF (problem)
        #and
        #filter_unsatisfiable (problem)
        #filter_satisfiable (problem)
        #filter_unknown (problem)
        #filter_open (problem)
        #and
        filter_non_equality (problem)
        #filter_equality (problem)
        #and
        #filter_hne (problem)
        #filter_nne (problem)
        #and
        #(
        #filter_FOF (problem)
        #or
        #not (filter_bernays_schoenfinkel (problem))
        #)
        #and
        #filter_rating (problem)
        )
    



# check every file if it's a wanted problem description
def check_problem_file (problems, dir_name, files_in_dir):
    for file_name in files_in_dir:
        file_path = os.path.join (dir_name, file_name)

        if ((file_name[-len(SUFFIX):] == SUFFIX)
            and
            (os.path.isfile (file_path))):
            try:
                file = open (file_path)

                problem = []
                line = file.readline ()
                while (line):
                    match1 = re.search ("^%", line)
                    match2 = re.search ("^$", line)
                    if not (match1 or match2):
                        break

                    problem.append (line)
                    line = file.readline ()
                    
                #problem = file.readlines ()
                
                file.close ()

                if filter (problem):
                    problems.append (file_name)

            except IOError, (error_nr, error_string):
                print ("Couldn't open " + file_path + ": " + error_string)



# get all wanted problems and print them line by line
problems = []
os.path.walk (TPTP_PATH, check_problem_file, problems)
problems.sort ()
for problem in problems:
    # remove the SUFFIX
    print problem[:-len(SUFFIX)]
