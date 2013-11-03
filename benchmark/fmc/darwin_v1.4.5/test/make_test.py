#! /usr/bin/python

import sys
import os
import string



projects = {}

projects["tools"] = ["tools"]

projects["var"] = ["var"]

projects["symbol"] = ["counter", "symbol"]

projects["read_darwin"] = ["counter", "config", "statistic", "var", "symbol", "tools", "term",
                           "parser_darwin", "lexer_darwin", "read_darwin"]

projects["read_tme"] = ["counter", "config", "statistic", "var", "symbol", "tools", "term",
                        "parser_darwin", "lexer_darwin", "read_darwin",
                        "parser_tme", "lexer_tme", "read_tme"]


projects["term"] = ["counter", "config", "statistic", "var", "symbol", "tools", "term",
                    "parser_darwin", "lexer_darwin", "read_darwin", "parser_tme", "lexer_tme", "read_tme"]

projects["subst"] = ["counter", "config", "statistic", "var", "symbol", "tools", "term", "subst",
                             "parser_darwin", "lexer_darwin", "read_darwin",
                             "parser_tme", "lexer_tme", "read_tme"]

projects["term_quality"] = ["counter", "config", "statistic", "var", "symbol", "tools", "term",
                            "subst", "term_quality",
                            "parser_darwin", "lexer_darwin", "read_darwin",
                            "parser_tme", "lexer_tme", "read_tme"]

projects["unification"] = ["counter", "config", "statistic", "var", "symbol", "tools", "term", "subst", "unification",
                             "parser_darwin", "lexer_darwin", "read_darwin",
                             "parser_tme", "lexer_tme", "read_tme"]


projects["substitution_tree"] = ["counter", "config", "statistic", "var", "symbol", "tools", "term",
                                 "subst", "unification", "term_indexing", "substitution_tree",
                                 "parser_darwin", "lexer_darwin", "read_darwin",
                                 "parser_tme", "lexer_tme", "read_tme"]

projects["discrimination_tree"] = ["const", "print", "tools", "counter", "var", "symbol", "term", "flags", "config", "statistic",
                                   "subst", "term_attributes", "unification", "term_indexing", "discrimination_tree",
                                   "parser_darwin", "lexer_darwin", "read_darwin",
                                   "parser_tme", "lexer_tme", "read_tme"]

projects["disc_tree"] = ["print", "counter", "flags", "config", "statistic", "var", "symbol", "tools", "term",
                                   "subst", "unification", "term_indexing", "disc_tree",
                                   "parser_darwin", "lexer_darwin", "read_darwin",
                                   "parser_tme", "lexer_tme", "read_tme"]

def make_test (project, make_arguments):
    print "* Making " + project + "..."
    print
    create_test_make_file (project)

#    command = "rm -f test_" + project
#    os.system (command)
    
    command = "make -f Makefile.test " + string.join (make_arguments, " ")
    print command
    if os.system (command):
        sys.exit()

    command = "rm Makefile.test"
    os.system (command)

    print
    

def make_tests (make_arguments):
    print
    print "** Making all tests..."
    print
    print
    for project in projects:
        make_test (project, make_arguments)


def perform_test (project):
    print "*Testing... ",
    command = "./test_" + project
    print command
    os.system (command)
    print


def perform_tests ():
    print
    print "** Performing tests..."
    print
    print
    for project in projects:
        perform_test (project)



def create_test_make_file (project):
    target_name = "test_" + project

    try:
        project_sources = projects[project]

        sources = map (lambda project: "../src/" + project + ".ml", project_sources)
        
        file = open ("Makefile.test", 'w')
        file.write ("OCAMLMAKEFILE = ../OCamlMakefile\n")
        file.write ("\n")
        file.write ("OCAMLC := ocamlc.opt\n")
        file.write ("OCAMLOPT := ocamlopt.opt\n")
        file.write ("LIBS = unix\n")
        file.write ("\n")
        file.write ("SOURCES = " + string.join (sources) + " oUnit.ml test_" + project + ".ml" + "\n")
        file.write ("\n")
        file.write ("RESULT  = " + target_name + "\n")
        file.write ("\n")
        file.write ("-include $(OCAMLMAKEFILE)\n")
        file.close ()
        
    except KeyError:
        print "Unknown project: " + target_name
        sys.exit()





# change to darwin's test dir
if sys.path[0] <> "" :
    os.chdir (sys.path[0])
    

# make
# make clean ...
# make test ...
# make buffy ...
# make test clean ...
if (len(sys.argv) > 1):
    if (sys.argv[1] == "all"):
        # make and perform all tests
        make_tests (sys.argv[2:])
        perform_tests ()

    elif (sys.argv[1] == "clean"):
        # clean everything
        make_tests (sys.argv[1:])

    elif (sys.argv[1][0:5] == "test_"):
        # make a specific project
        make_test (sys.argv[1][5:], sys.argv[2:])
        perform_test (sys.argv[1][5:])

    else:
        # fail miserably
        print "I don't know what to do..."
                    
else:
    # make and perform all tests
    make_tests (sys.argv[1:])
    perform_tests ()
