#! /usr/bin/python

import os
import sys
import getopt
import re

# configuration script for darwin
# creates a Makefile from Makefile.in


# global configuration, filled as detected
env = {}


# files needed to compile
files_required = [
    "ocamldep",
    "ocamlc",
    "ocamlopt",
    "ocamllex",
    "ocamlyacc",
    ]

# optional files
files_optional = [
    "ocamldoc",
    "ocamlfind",
    ]




# find a program in the system path
def find_program (path, name):
    for path in path.split(":"):
        combined = os.path.join(path, name)
        if os.path.isfile(combined):
            combined = re.sub("([^\\\\]) ", "\\1\\ ", combined)
            return combined

# detect the OCAML tools
def find_programs(path, programs):
    for file in programs:
        # try to find the native code version
        found = find_program(path, file + ".opt")
        if found:
            env[file] = found
        # use bytecode instead
        else:
            found = find_program(path, file)
            if found:
                env[file] = found


# check that all requires files are found
def check_required(files):
    for file in files_required:
        if not file in env:
            print ("Aborting: Could not find " + file + " in system path.")
            sys.exit()


# check that ocaml version >= 3.08
def check_ocaml_version():
    file = os.popen(env["ocamlc"] + " -version")
    version = (file.readline())[:-1]
    file.close()
    env["VERSION"] = version
    # require version 3.08
    if version < "3.08":
        print ("Aborting: OCaml version >= 3.08 required, found " + version + ".")
        sys.exit()
    # disable new warning flags for 3.08
    elif version < "3.09":
        env["WARNING_FLAGS"] = "Ae"
    # enable everything for >= 3.09
    else:
        env["WARNING_FLAGS"] = "Aez"

    

# detect zip support
def detect_zip():
    # by default no zip support
    env["zip"] = ""

    # use ocamlfind to find the zip library
    if env.has_key("ocamlfind"):
        # ocaml uses zip, godi uses camlzip
        for file in ["zip", "camlzip"]:
            if os.system(env["ocamlfind"] + " query -p-format " + file + " >& /dev/null") == os.EX_OK:
                # zip library found
                file = os.popen(env["ocamlfind"] + " query -p-format " + file)
                found = file.readline()
                file.close()
                env["zip"] = found[:-1] # cut newline
                return


# show the user the determined configuration
def print_configuration():
    width = 16
    print "Configuration successful:"

    print "OCaml version".ljust(width) + " : " + env["VERSION"]
    for file in ["ocamlc", "ocamlopt", "ocamllex", "ocamlyacc", "ocamldep", "ocamldoc", "ocamlfind"]:
        if env.has_key(file):
            print file.ljust(width) + " : " + env[file]
        else:
            print file.ljust(width) + " : " + "not found"

    zip = env["zip"]
    if zip:
        print "zip".ljust(width) + " : " + env["zip"]
    else:
        print "zip".ljust(width) + " : " + "no support for zipped input files."

    print


# create Makefile
def create_make():
    print "creating Makefile"

    file = open("Makefile", 'w')

    # write configuration to new make file
    file.write("# created by configure.py\n")
    for program in ["ocamlc", "ocamlopt", "ocamllex", "ocamlyacc", "ocamldep", "ocamldoc", "ocamlfind", "zip"]:
        if env.has_key(program):
            file.write(program.upper() + " := " + env[program] + "\n")
    # warning options based on ocaml version
    file.write("WARNING_FLAGS := " + env["WARNING_FLAGS"] + "\n")

    # append template make file
    try:
        template = open("Makefile.in", 'r')
        for line in template.readlines():
            file.write(line)
        template.close()        
    except IOError, (error_nr, error_string):
        print ("Couldn't open template make file Makefile.in: " + error_string)
        sys.exit()

        
    file.close()


def print_help():
    print "Configuration script for the darwin prover."
    print "Tries to automatically find the ocaml tools"
    print "and to create a corresponding Makefile."
    print
    print "The only option is --path=<PATH> ,"
    print "which allows to specify the path in which the ocaml tools should be searched for."

# main
def main():
    if sys.version < '2.3':
        print "Warning: python version 2.3 needed for all functionality."
        print

    # by default the search path
    path = os.getenv("PATH")

    # check if search path for ocaml has been given
    (opts, args) = getopt.getopt(sys.argv[1:], "h", ["help", "path="])

    for (option, value) in opts:
        if ((option == "--help") or (option == "-h")):
            print_help()
            sys.exit(0);
        if (option == "--path"):
            path = value

    find_programs(path, files_required)
    find_programs(path, files_optional)
    check_required(files_required)
    check_ocaml_version()
    detect_zip()
    print_configuration()
    create_make()


main()
