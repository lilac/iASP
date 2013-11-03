#! /usr/bin/env python
# -*- coding: utf-8 -*-

#
# Converts flat clauses to ivASP logic program 
# for computing finite models
#
# James Deng
# 2013
#
# Orkunt Sabuncu 
# orkunt@cs.uni-potsdam.de
# orkunts@gmail.com
#
# 2010
#


import sys
import re

cnf_main = re.compile('cnf\(([^,]+),([^,]+),(.+)\).')
int_term_assign = re.compile('int__fd_r(\d+)\((.+)\)')
term_constant = re.compile('int__fd_s_(.+)')
int_predicates = re.compile('int__([^_]+)\((.+)\)')
int_predicatesArity0 = re.compile('int__([^_]+)$')
inp_preds = re.compile('([^ ]+)\((.+)\)')
inp_predsArity0 = re.compile('([^ ()]+)$')

internal_predicates = set([])
input_predicates = set([])
input_constants = set([])
input_functions = set([])

def find_cnf_formula():
  cnf_formula = ""

  while True:
    l = sys.stdin.readline()
    if l == '':
      return None
    r = re.match('cnf\(', l)
    if not r:
      continue
    cnf_formula += l[:-1]
    while True:
      l = sys.stdin.readline()
      if l == '':
	sys.stderr.write('Parsing error:\n')
	sys.stderr.write('%s\n' %(cnf_formula))
	sys.exit(-1)
      r = re.search('\)\.', l)
      cnf_formula += l[:-1]
      if r:
	break
    break

  return cnf_formula


def parseFormulaTPTPCNF(formulastr):
  r = cnf_main.search(formulastr)
  if r:
    # process the clause and form a list of predicates
    if r.group(3).strip()[0] == '(':
      c = r.group(3).strip()[1:-1]
    else:
      c = r.group(3).strip()

    cla = map(lambda x: x.strip(), c.split('|'))

    return (r.group(1).strip(),r.group(2).strip(), cla)

  else:
    sys.stderr.write('Parsing error:\n')
    sys.stderr.write('%s\n' %(formulastr))
    sys.exit(-1)


def parseFileTPTPCNF():
  while True:
    o = find_cnf_formula()
    if o == None:
      return
    else:
      cnf_f = parseFormulaTPTPCNF(o)
      if cnf_f[0].startswith('flattened'):
	#print cnf_f
	clauseToConstraint(cnf_f, cnf_f[0])


def clauseToConstraint(cla,name):
  constraint = [name]
  variables = set([])
  positive_lit = True

  for lit in cla[2]:
    # Cv~B => :- not C, B
    if lit[0] == '~':
      positive_lit = True
    else:
      positive_lit = False

    # check the type of the literal
    # int__fd_r1 assigning terms
    r = int_term_assign.search(lit)
    if r:
      arity = int(r.group(1))
      #print 'arity', arity
      args = map(lambda x: x.strip(), r.group(2).split(','))
      #print 'args', args
      # term constants are in the form int__fd_s_XXX
      termcons = term_constant.search(args[0]).group(1)
      #print 'termcons', termcons
      if arity == 0:
	lplit = 'assign(%s,%s)'%(termcons,args[1])
	input_constants.add(termcons)
      else:
	lplit = 'assign(%s(%s),%s)' %(termcons,reduce(lambda x,y: x+','+y, args[1:-1]), args[-1])
	input_functions.add((termcons,arity))
      constraint.append((positive_lit,lplit))
      variables |= set(args[1:])
      continue

    #elif:
    # check int__conXX and int__diff literals
    r  = int_predicates.search(lit)
    if r:
      intpredname = r.group(1)
      args = map(lambda x: x.strip(), r.group(2).split(','))
      variables |= set(args)
      if intpredname == 'diff':
	constraint.append((positive_lit,args[0]+'!='+args[1]))
	continue
      internal_predicates.add((intpredname,len(args)))
      lplit = '%s(%s)' %(intpredname,reduce(lambda x,y: x+','+y, args))
      constraint.append((positive_lit,lplit))
      sys.stderr.write('Processed %s\n' %(intpredname))
      continue

    # check internal propositional predicates
    r = int_predicatesArity0.search(lit)
    if r:
      intpredname = r.group(1)
      internal_predicates.add((intpredname,0))
      constraint.append((positive_lit, intpredname))
      sys.stderr.write('Processed %s\n' %(intpredname))
      continue

    #else:
    # should be predicates from the input
    # of arity>0
    r = inp_preds.search(lit)
    if r:
      inppredname = r.group(1)
      args = map(lambda x: x.strip(), r.group(2).split(','))
      variables |= set(args)
      input_predicates.add((inppredname,len(args)))
      lplit = r.group(0)
      constraint.append((positive_lit,lplit))
      continue

    # propositional predicates
    r = inp_predsArity0.search(lit)
    if r:
      inppredname = r.group(1)
      input_predicates.add((inppredname,0))
      constraint.append((positive_lit,inppredname))
      continue

    sys.stderr.write('COULD NOT PROCESS: %s\n' %(lit))

  #constraint.append(list(variables))

  #print constraint
  # output the constraint rule to stdout
  # is it ground rule?
  if len(variables) == 0:
    sys.stdout.write('%% %s\n' %(constraint[0]))
    sys.stdout.write(':- ')
    for i in range(1,len(constraint)):
      if not constraint[i][0]:
	sys.stdout.write('not ')
      sys.stdout.write('%s'%(constraint[i][1]))
      if i == len(constraint)-1:
	sys.stdout.write('.\n\n')
      else:
	sys.stdout.write(', ')
  else:
      sys.stdout.write('%% %s\n' %(constraint[0]))
      sys.stdout.write(':- ')
      for i in range(1,len(constraint)):
	if not constraint[i][0]:
	  sys.stdout.write('not ')
	sys.stdout.write('%s, '%(constraint[i][1]))
      args = reduce(lambda x,y: x+';'+y, list(variables))
      sys.stdout.write('dom(%s).\n\n' %(args))


def main():
  parseFileTPTPCNF()

  #print internal_predicates
  #print input_predicates
  #print input_functions
  #print input_constants

  # input functions
  sys.stdout.write('\n% functions\n')
  for f in input_functions:
    args = reduce(lambda x,y:x+','+y, map(lambda x: 'X'+str(x), range(0,f[1])))
    args2 = reduce(lambda x,y:x+';'+y, map(lambda x: 'X'+str(x), range(0,f[1])))
    fn = '%s(%s)' %(f[0], args)
    sys.stdout.write('func(%s) :- dom(%s).\n' %(fn,args2))
    #sys.stdout.write('assign(%s,Y) :- dom(Y), func(%s), not nassign(%s,Y).\n' %(fn,fn,fn))
    #sys.stdout.write('nassign(%s,Y) :- dom(Y), assign(%s, Y1), Y1 != Y.\n' %(fn,fn))

  # internal predicates
  sys.stdout.write('\n% internal predicates\n')
  for f in internal_predicates:
    if f[1] == 0:
      sys.stdout.write('{ %s }.\n' %(f[0]))
    else:
      args = reduce(lambda x,y:x+','+y, map(lambda x: 'X'+str(x), range(0,f[1])))
      args2 = reduce(lambda x,y:x+';'+y, map(lambda x: 'X'+str(x), range(0,f[1])))
      sys.stdout.write('{ %s(%s) } :- dom(%s).\n' %(f[0],args,args2))

  # input predicates
  sys.stdout.write('\n% input predicates\n')
  for f in input_predicates:
    if f[1] == 0:
      sys.stdout.write('{ %s }.\n' %(f[0]))
    else:
      args = reduce(lambda x,y:x+','+y, map(lambda x: 'X'+str(x), range(0,f[1])))
      args2 = reduce(lambda x,y:x+';'+y, map(lambda x: 'X'+str(x), range(0,f[1])))
      sys.stdout.write('{ %s(%s) } :- dom(%s).\n' %(f[0],args,args2,))

  # input constants
  order = 0
  for f in input_constants:
    order += 1
    sys.stdout.write('cons(%s).\n' %(f))
    sys.stdout.write('order(%s,%d).\n' %(f,order) )

  sys.stdout.write('\n#hide.\n#show assign/2.\n')
  for f in input_predicates:
    sys.stdout.write('#show %s/%d.\n' %(f[0],f[1]))
  sys.stderr.write('Conversion to ivASP done.\n')

if __name__ == "__main__":
    main()
                      
