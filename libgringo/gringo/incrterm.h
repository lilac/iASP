// Copyright (c) 2009, James Deng <cnjamesdeng@gmail.com>
//
// This file is part of gringo.
//
// gringo is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// gringo is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with gringo.  If not, see <http://www.gnu.org/licenses/>.

#pragma once

#include <gringo/gringo.h>
#include <gringo/term.h>
#include <gringo/grounder.h>
#include <gringo/prgvisitor.h>

class IncrTerm : public Term
{
public:
	IncrTerm(const Loc &loc) : Term(loc) {}
	Val val(Grounder *grounder) const { return Val::create(Val::NUM, grounder->level()); }
	void normalize(Lit *parent, const Ref &ref, Grounder *grounder, Expander *expander, bool unify) { (void)parent; (void)ref; (void)grounder; (void)expander; (void)unify; }
	bool unify(Grounder *grounder, const Val &v, int binder) const { return true; }
	void vars(VarSet &vars) const {}
	void visit(PrgVisitor *visitor, bool bind) { visitor->visit(this); }
	void print(Storage *sto, std::ostream &out) const { out << "#iVar"; }
	bool constant() const { return false; }
	Term *clone() const { return new IncrTerm(*this); }
};

