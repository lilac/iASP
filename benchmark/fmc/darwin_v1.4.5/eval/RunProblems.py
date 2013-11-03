#! /usr/bin/python

import sys
import getopt
import os
import resource
import select
import re
import math
import zipfile

###
### Constants
###


# RESOURCE LIMITS
TIME_LIMIT = 1#10 * 60 # s (CPU time, not wall clock)
MEMORY_LIMIT = 1000 # MB

# VARIABLES

# use zipped archive containing problems
#ZIP_FILE = "../TPTP-v3.0.1.zip"
#TPTP_PATH = ""
#PROVER = "./darwin" + " -to " + str (TIME_LIMIT) + " -lm false -rs Lazy -pc false -zip " + ZIP_FILE

# use unzipped input
ZIP_FILE = 0
TPTP_PATH = "/mnt/data/TPTP-v3.1.1/protein/"
TPTP_PATH = "/media/data/DARWIN/DATA/TPTP-v3.2.0/tptp/"
TPTP_PATH = "/media/shared/Benchmarks/TPTP-v4.0.0/Problems/"
PROVER = "./darwin" + " -to " + str (TIME_LIMIT) + " -pc false "
#PROVER = "./darwin" + " -to " + str (TIME_LIMIT) + " -fd false -rs Delayed -pc false "
#PROVER = "./darwin" + " -to " + str (TIME_LIMIT) + " -fd true -pppr true -pc false "
#PROVER = "./darwin" + " -to " + str (TIME_LIMIT) + " -fd true -pc false -pmtptp true "

# use zipped input
#ZIP_FILE = 1
#TPTP_PATH = "/mnt/data/TPTP-v3.0.1-each-zipped/"
#PROVER = "./darwin" + " -to " + str (TIME_LIMIT) + " -lm false -rs Delayed -pc false -db 1 -fm true -pps false"

#TPTP_PATH = "/mnt/data/TPTP/"
#TPTP_PATH = "/home/alexf/TPTP/Problems/protein/"
#PROVER = "../darwin" + " -to " + str (TIME_LIMIT) + " -lm false -rs Lazy -pc false -rj true"
#PROVER = "../darwin -umx true -re false -fam false"
#PROVER = "../darwin -b 2 -ubj true -uv false -vd true -vcu true -vm true -vst true"

# FILE SUFFIXES
PROBLEMS_SUFFIX = "" #"-problems"
OUTPUT_SUFFIX = "-output"
SUMMARY_SUFFIX = "-summary"



# compiles the regular expressions in a (key, pattern) array
def compile_table (table):
    return map (lambda (key, pattern): (key, re.compile (pattern)), table)


# STATUS KEYWORDS
UNSATISFIABLE = "unsat"
THEOREM = "theorem"
COUNTERSATISFIABLE = "cosat"
SATISFIABLE = "sat"
TIMEOUT = "timeout"
RESOURCE = "resout"
STATUS_KEYS = compile_table (
    [
    (UNSATISFIABLE, "^SZS status Unsatisfiable"),
    (THEOREM, "^SZS status Theorem"),
    (COUNTERSATISFIABLE, "^SZS status CounterSatisfiable"),
    (SATISFIABLE, "^SZS status Satisfiable"),
    (TIMEOUT, "^SZS status Timeout"),
    (RESOURCE, "^SZS status ResourceOut"),
    #(TIMEOUT, "^SZS status: ResourceOut"),
    # OCaml specific
    ("crash", "Fatal error:"),
    # Linux (kernel) specific?
    ("memory", "Fatal error: out of memory."),
    ]
    )

# STATISTIC KEYWORDS
# (Name, Pattern)
#
# Pattern must contain a group with the symbolic name <value>
#
# the summary contains a column 'Name'
#
# if the prover outputs a line matching against a pattern
# the value is printed in the corresponding column
STATISTIC_KEYS = compile_table (
    [
    ("Close", "Close\s+:\s+(?P<value>\d+)"),
    ("Assert", "Assert\s+:\s+(?P<value>\d+)"),
    ("Split", "Split\s+:\s+(?P<value>\d+)"),
#    ("Subsume", "Subsume\s+:\s+(?P<value>\d+)"),
#    ("Resolve", "Resolve\s+:\s+(?P<value>\d+)"),
#    ("Compact", "Compact\s+:\s+(?P<value>\d+)"),
#    ("Prod.", "Productivity Filtered\s+:\s+(?P<value>\d+)"),
    ("D.Bound", "Bound\s+:\s+(?P<value>\d+)"),
    ("Ct.Size", "Maximum Context Size\s+:\s+(?P<value>\d+)"),
    ("Ass.Cd.", "Assert Candidates\s+:\s+(?P<value>\d+)"),
    ("Spl.Cd.", "Split Candidates\s+:\s+(?P<value>\d+)"),
    ("Lemmas", "Lemmas\s+:\s+(?P<value>\d+)"),
    ("Deb1", "Global Debug\s+:\s+(?P<value>\d+)"),
    ("Deb2", "Global Debug2\s+:\s+(?P<value>\d+)"),
    ("CPU", "CPU Time\s+:\s+(?P<value>\d+\.\d+)"),
    ]
    )


# for summary
PROBLEM_COL_WIDTH = 16
DEFAULT_COL_WIDTH = 8



###
### functions
###


  

# set hard limits for time and memory usage
# as the prove is run in a subprocess
# it is simply killed when exceeding these hard limits
def set_resource_limits ():
    #time_limit = TIME_LIMIT
    memory_limit = MEMORY_LIMIT * 1024 * 1024
    #resource.setrlimit (resource.RLIMIT_CPU, (time_limit, time_limit))
    resource.setrlimit (resource.RLIMIT_AS, (memory_limit, memory_limit))



# read the problems from the files given as command line arguments
# and return the problem list per file
def read_problem_files (problem_files):
    problems = map (read_problem_file, problem_files)
    return map (None, problem_files, problems)

# return the list of problems in the file
def read_problem_file (file_name):
    try:
        file = open (file_name + PROBLEMS_SUFFIX)
        problems = file.readlines ()
        file.close ()
        return problems

    except IOError, (error_nr, error_string):
        print ("Couldn't open " + file_name  + PROBLEMS_SUFFIX + ": " + error_string)
        return []
    

# run the prover on the problem sets
def run_problem_suites (resume, problem_suites):
    for (problem_file_name, problems) in problem_suites:
        if len (problems) > 0:
            if resume:
                mode = "a"
            else:
                mode = "w"
                
            output_file = open (problem_file_name + OUTPUT_SUFFIX, mode)
            summary_file = open (problem_file_name + SUMMARY_SUFFIX, mode, 1)

            if not resume:
                print_header (problem_file_name, output_file, summary_file)

            for problem in problems:
                # remove trailing newline
                problem = problem[:-1]
                if problem == resume:
                    resume = ""

                elif resume == "":
                    run_problem (problem, output_file, summary_file)

            output_file.write ("\n")
            summary_file.write ("\n")
            print ""

            output_file.close ()
            summary_file.close ()


# builds the full path to the problem
# e.g. SYN036-3 -> ./TPTP/protein/SYN/SYN036-3.tme
def build_problem_file_path (problem):
    #return TPTP_PATH + problem[0:3] + "/" + problem + ".tme"
    return TPTP_PATH + problem[0:3] + "/" + problem + ".p"

def build_zip_file_path (problem):
    return TPTP_PATH + problem[0:3] + "/" + problem + ".tme.zip"

def build_zip_problem_file_path (problem):
    return problem + ".tme"


# print the header for a lost of problem summaries
def print_header (problem_file_name, output_file, summary_file):
    # configuration
    header = ""
    header = header + "--------------------------------------------------------------------------------" + "\n"
    header = header + "Execute format string : " + PROVER + "\n"
    header = header + "Problems list file    : " + problem_file_name + PROBLEMS_SUFFIX + "\n"
    header = header + "Output file           : " + problem_file_name + OUTPUT_SUFFIX + "\n"
    header = header + "Summary file          : " + problem_file_name + SUMMARY_SUFFIX + "\n"
    header = header + "Time limit            : " + str (TIME_LIMIT) + " s" + "\n"
    header = header + "Memory limit          : " + str (MEMORY_LIMIT) + " MB" + "\n"

    output_file.write (header)
    summary_file.write (header)
    print header,


    # header for summary
    summary_header = "--------------------------------------------------------------------------------" + "\n"
    summary_header = summary_header + "Problem".ljust (PROBLEM_COL_WIDTH)
    summary_header = summary_header + "Result".rjust (DEFAULT_COL_WIDTH)
    summary_header = summary_header + "CPU".rjust (DEFAULT_COL_WIDTH)
    summary_header = summary_header + "Memory".rjust (DEFAULT_COL_WIDTH)
    
    for (key, pattern) in STATISTIC_KEYS:
        if key <> "CPU":
            summary_header = summary_header + key.rjust (DEFAULT_COL_WIDTH)

    summary_file.write (summary_header + "\n")
    print summary_header
        



# contains the summary of a problem run -
# status, time, memory, statistic, ...
class ProblemSummary:
    problem = ""
    result = "crash"
    memory = 0
    start_time = 0
    end_time = 0
    statistic = {}
    
    def __init__ (self, problem):
        self.problem = problem
        self.result = "crash"
        self.memory = 0
        self.start_time = self.get_children_time ()
        self.end_time = self.start_time
        self.statistic = {}


    #
    # helper functions
    #

    # how much time have the children processes consumed?
    def get_children_time (self):
        (_, _, child_system_time, child_user_time, _) = os.times ()
        return child_system_time + child_user_time


    # time used by the current child
    # to be run after update_end_time ()
    def used_time (self):
      return math.floor (((self.end_time - self.start_time) * 10) + 0.5) / 10


    # is the problem satisfiable, unsatisfiable?
    # read from the problem specification
    def problem_status (self):
        # no try clause - if we fail, we want to crash

        # no zipped input
        if not ZIP_FILE:
            file = open (build_problem_file_path (self.problem))

            for line in file.readlines ():
                match = re.search ("^% Status   : (?P<value>.*)$", line)
                if match:
                    file.close ()
                    if match.group ("value") == "Unsatisfiable":
                        return UNSATISFIABLE

                    elif match.group ("value") == "Theorem":
                        return THEOREM

                    elif match.group ("value") == "Satisfiable":
                        return SATISFIABLE

                    elif match.group ("value") == "CounterSatisfiable":
                        return COUNTERSATISFIABLE

                    else:
                        return ""
                    
                    
            file.close ()
            return ""

        # zipped input
        else:
            zip_file = zipfile.ZipFile (build_zip_file_path (self.problem))
            problem = zip_file.read (build_zip_problem_file_path (self.problem))
            zip_file.close ()
            lines = problem.split ("\n")
            for line in lines:
                match = re.search ("^% Status   : (?P<value>.*)$", line)
                if match:
                    if match.group ("value") == "Unsatisfiable":
                        return UNSATISFIABLE

                    elif match.group ("value") == "Theorem":
                        return THEOREM

                    elif match.group ("value") == "Satisfiable":
                        return SATISFIABLE

                    elif match.group ("value") == "CounterSatisfiable":
                        return COUNTERSATISFIABLE

                    else:
                        return ""
           
    
            


    #
    # updates
    #

    # to be run after the child process, i.e. the prover, is finished
    def update_end_time (self):
        self.end_time = self.get_children_time ()


    # updates the maxmimum memory usage of the process with process id pid in MB
    # reads the information from the /proc file: Linux specific
    def update_memory (self, pid):
        try:
            for line in open ("/proc/" + str (pid) + "/status").readlines () :
                match = re.search ("^VmSize:.*?(?P<value>\d+) kB", line)
                if match :
                    self.memory = max (self.memory, int (match.group ("value")) / 1024)

        except IOError, (error_nr, error_string):
            ()


    # the status of the problem - i.e. solved, crashed, timeout, out of memory, ...
    def update_status (self, line):
        for (key, pattern) in STATUS_KEYS:
            match = pattern.search (line)
            if match:
                self.result = key


    # search for statistic information
    def update_statistic (self, line):
        for (key, pattern) in STATISTIC_KEYS:
            match = pattern.search (line)
            if match:
                try:
                    self.statistic[key] = match.group ("value")

                except IndexError:
                    print "Pattern " + pattern + " is invalid: does not contain group <value>"



    #
    # representation
    #

    # result, or unsound if the result differs from the expected result
    # ignore check for unknown and open problems
    def result_to_string (self):
        problem_status = self.problem_status ()
        if (
            (self.result in [UNSATISFIABLE, THEOREM, SATISFIABLE, COUNTERSATISFIABLE])
            and
            (problem_status in [UNSATISFIABLE, THEOREM, SATISFIABLE, COUNTERSATISFIABLE])
            and
            (self.result != problem_status)
            ):
            return "unsound"

        else:
            return self.result
            
                                                                       
    # the summary for a problem
    def to_string (self):
        to_string = ""
        to_string = to_string + self.problem.ljust (PROBLEM_COL_WIDTH)
        to_string = to_string + (self.result_to_string ()).rjust (DEFAULT_COL_WIDTH)
        #to_string = to_string + (str (self.used_time ())).rjust (DEFAULT_COL_WIDTH)
        if "CPU" in self.statistic:
            to_string = to_string + (str (self.statistic["CPU"])).rjust (DEFAULT_COL_WIDTH)
        else:
            to_string = to_string + (str (self.used_time ())).rjust (DEFAULT_COL_WIDTH)
            #to_string = to_string + "".rjust (DEFAULT_COL_WIDTH)
        to_string = to_string + (str (self.memory)).rjust (DEFAULT_COL_WIDTH)
        
        for (key, _) in STATISTIC_KEYS:
            if key in self.statistic:
                if key <> "CPU":
                    to_string = to_string + (str (self.statistic[key])).rjust (DEFAULT_COL_WIDTH)

            else:
                to_string = to_string + "".rjust (DEFAULT_COL_WIDTH)

        return to_string





# the prover is run in a child process
# connected by a pipe to the parent
def run_prover (reader, writer, problem) :
    set_resource_limits ()

    # redirect output and error messages
    os.close (reader)
    os.dup2 (writer, sys.stdout.fileno ())
    os.dup2 (writer, sys.stderr.fileno ())

    # run the prover
    if not ZIP_FILE:
        problem_file_name = build_problem_file_path (problem)
        #command_list = PROVER.split () + [problem + ".fd.model"] + [problem_file_name]
        command_list = PROVER.split () + [problem_file_name]
    else:
        problem_file_name = build_zip_problem_file_path (problem)
        command_list = PROVER.split () + ("-zip " + build_zip_file_path (problem)).split () + [problem_file_name]

    print "--------------------------------------------------------------------------------"
    print ("Executing: " + " ".join (command_list))
    
    try:
        os.execvp (command_list[0], command_list)

    except OSError, (error_nr, error_string):
        print ("Error in executing '" + " ".join (command_list) + "': " + error_string)
        sys.exit (error_nr)



# run the prover on a problem
def run_problem (problem, output_file, summary_file):
    summary = ProblemSummary (problem)

    # run the prover in a child process
    # connect its output to the parent process by a pipe
    reader, writer = os.pipe()
    pid = os.fork ()

    if pid == 0:
        run_prover (reader, writer, problem)
        
    else:
        os.close (writer)
        prover_output = os.fdopen(reader, 'r')

        # block with select on the provers output
        # timeout so that the memory usage can be updated
        while 1:
            (triggered, _, _) = select.select ([prover_output], [], [], 1)

            summary.update_memory (pid)

            # output from prover?
            if prover_output in triggered:
                # yes, so read it
                output_line = prover_output.readline ()

                if output_line != '':
                    #print output_line
                    # prover still running
                    output_file.write (output_line)
                    
                    summary.update_status (output_line)
                    summary.update_statistic (output_line)

                else:
                    # prover terminated
                    break
            else:
                # no, just a timeout to update the memory usage
                ()

        os.wait ()
        #os.close (reader)
            

    summary.update_end_time ()
    summary_string = summary.to_string ()
    print (summary_string)
    summary_file.write (summary_string + "\n")









###
### main
###

if (len (sys.argv) < 2):
    print ("USAGE: RunProblems.py <ProblemSet1> <ProblemSet2> <...>")
    print
    print ("Example: 'RunProblems.py SYN'")
    print ("  runs the solver on all TPTP problems in SYN" + PROBLEMS_SUFFIX + ",")
    print ("  writes the summary to SYN" + SUMMARY_SUFFIX + ",")
    print ("  and pipes the provers output to SYN" + OUTPUT_SUFFIX + ",")
else:
    optlist, args = getopt.getopt(sys.argv[1:], 'r:')

    # resume option given?
    resume_option = ""
    for (option, value) in optlist:
        if option == "-r":
            if len (args) > 1:
                print "only one suite if -r is given"
                sys.exit (0)
                
            resume_option = value
#    problem_suites = read_problem_files (sys.argv[1:])
    problem_suites = read_problem_files (args)
    run_problem_suites (resume_option, problem_suites)
