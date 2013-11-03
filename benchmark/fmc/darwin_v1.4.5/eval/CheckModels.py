#! /usr/bin/python

import sys
import os
import re


TPTP_PATH = "/mnt/data/TPTP-v3.1.1/tptp/"

files = sys.argv[1:]

for file_name in files:
    file_name_model = file_name + ".fd.model"

    if os.path.exists(file_name_model):
        print
        print file_name

        # read original problem
        file_name_problem = TPTP_PATH + file_name[:3] + "/" + file_name + ".tptp"
        file = open (file_name_problem)
        problem = file.readlines()
        file.close ()

        # read model
        file = open (file_name_model)
        lines = file.readlines()
        # replace labels as eprover does not understand them
        model = map(lambda line: re.sub("fi_(.*),", "axiom,", line), lines)
        file.close ()

        # ignore problem if want to check only model for consistency
        #problem = []

        # create combined problem
        file_name_combined = file_name + ".combined.model"
        file = open (file_name_combined, 'w')
        file.writelines(problem)
        file.writelines(model)
        file.close ()

        # run prover
        sys.stdout.flush()
        os.system ("eprover --cpu-limit=30 --memory-limit=200 --tstp-format --silent " + file_name_combined)
    
        # remove file - takes too much space
        os.unlink (file_name_combined)
            
