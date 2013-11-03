#! /usr/bin/python

import os


problem_file = "/mnt/data/TPTP-v3.1.1/tptp/GEO/GEO153-1.tptp"
context_file = "context"

# read original problem : T
file = open (problem_file)
problem = file.readlines()
file.close ()
#print "".join(problem)


# read context:
#   L1, L2, ..., Ln
file = open (context_file)
literals = map(lambda x: x[:-1], file.readlines())
file.close ()
#print "\n".join(literals)


# create n proof tasks:
#   T \cup { L_1, ..., L_{i-1} } |= \forall L_i
for i in xrange(0, len(literals)):
     #print i
     #print literals[i]
     file_name = "proof_" + repr(i) + ".tptp"
     print file_name
     file = open (file_name, "w")

     # problem
     file.writelines(problem)
     file.write("\n\n");

     # derived facts
     for j in xrange(0, i):
         # for eprover: no parenthesis for equality
         #if literals[j][0] == "(":
         #    line = "cnf(p_" + repr(j) + ",axiom,  " + literals[j] + ").\n" 
         #else:
         line = "cnf(p_" + repr(j) + ",axiom,( " + literals[j] + ")).\n"
         file.write(line)

     # conjecture
     # for eprover: no double negation
     if literals[i][0] == "~":
         line = "cnf(p_" + repr(i) + ",axiom,( " + literals[i][2:] + ")).\n"
     else:
         line = "cnf(p_" + repr(i) + ",axiom,( ~ " + literals[i] + ")).\n"
     file.write(line)

     file.close()

     # run eprover
     #os.system ("./darwin -pl 0 -to 10 " + file_name)
     #os.system ("eprover --cpu-limit=10 --memory-limit=300 --tstp-format --silent " + file_name)
    


# create refutation task:
#   { L_1, ..., L_{i-1}, C }
file_name = "proof_close.tptp"
print file_name
file = open (file_name, "w")

# problem
file.writelines(problem)
file.write("\n\n");

# derived facts
for i in xrange(0, len(literals)):
    line = "cnf(p_" + repr(i) + ",axiom,( " + literals[i] + ")).\n"
    file.write(line)

file.close()

#os.system ("./darwin -pl 0 -to 10 " + file_name)
#os.system ("eprover --cpu-limit=10 --memory-limit=300 --tstp-format --silent " + file_name)
