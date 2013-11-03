(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2006
              The University of Iowa

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*)

(** removes pure clauses

    A clause is pure in a clause set,
    if it contains a literal
    which does not unify with the negation of a literal in any other clause.

    For problems without equality this is detected by unification
    of the literals occuring in the clause set.

    For problems with equality by checking that a predicate symbol
    occurs only in one polarity.
 *)


(** {6 Types} *)

type literal = Term.literal
type clause = Term.clause



(** {6 Functions} *)

(** removes pure clauses.

    [print] prints removed pure clauses.
    [equality] denotes that the input contains equality.

    Is done up to fixpoint for equality,
    but due to the overhead only one pass is performed otherwise.

    returns the simplified clause set,
    the pure clauses removed from the original clause set.
    and the pure literals of the removed pure clauses.
*)
val simplify: print:bool -> equality:bool -> finite_domain:bool -> clause list ->
  clause list * clause list * literal list
