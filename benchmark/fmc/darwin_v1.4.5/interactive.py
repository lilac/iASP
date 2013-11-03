#! /usr/bin/python

# $Id: interactive.py,v 1.3 2004/06/13 18:22:21 alexf Exp $

# This file is part of the first order theorem prover Darwin
# Copyright (C) 2004  The University of Iowa
#                     Universitaet Koblenz-Landau 
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.


# NOTE: interactive operation of darwin is currently disabled,
# so this script does not work (as of Darwin 1.2 *)

# starts the ocaml toplevel with all darwin modules loaded
#
# Note: this is only possible with byte code, not native code,
# so darwin is significantly slower - from about 8x to 20x times.

# to use interactively from emacs tuareg mode use:
#  - C-c C-s inside an .ml buffer
#  and
#    - each time enter the path to this script
#    - or once append this line (adapted to your path to this script)
#      to emacs' configuration file:
#      (setq tuareg-interactive-program "~/darwin/interactive.py")


import sys
import os
import re


def find_sources_line (lines) :
    for i in xrange (0, len (lines) - 1):
        if re.search ("^SOURCES", lines[i]):
            return i

    raise "No SOURCES line found - don't know the modules to link."



def find_source_modules () :
    lines = open ("Makefile").readlines ()
    sources_line = find_sources_line (lines)
    
    modules = []
    for i in xrange (sources_line + 1, len (lines) - 1):
        new_modules = re.findall ("\S+\.ml", lines[i])
    
        if len (new_modules) == 0:
            return modules
    
        else:
            modules = modules + new_modules

    raise "No modules found - don't know the modules to link."



def module_to_byte_code (module):
    match = re.search ("(?P<module_name>.*\.)ml", module)
    if match:
        return match.group ("module_name") + "cmo"
    else:
        raise "module_to_byte_code: couldn't deduce .cmo name: " + module


def find_modules () :
    source_modules = find_source_modules ()
    return map (module_to_byte_code, source_modules)


# change to darwin's top dir
if sys.path[0] <> "" :
    os.chdir (sys.path[0])

print "making bytecode modules..."
print
os.system ("make bc")

command = ["ocaml", "-I", "src/"] + ["unix.cma"] + find_modules ()
print
print "Starting interactive toplevel..."
print " ".join (command)
print
os.execvp (command[0], command)
