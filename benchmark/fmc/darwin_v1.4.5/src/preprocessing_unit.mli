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

(** unit subsumption and resolution

    removes (non-unit) clauses subsumed by unit clauses,
    and simplifies clauses by removing literals resolved by unit clauses.
*)

(** {6 Types} *)

type clause = Term.clause

(** this clause is simplified to the empty clause,
    so the clause set is unsatisfiable. *)
exception EMPTY_CLAUSE of clause



(** {6 Functions} *)

(** performs unit simplification.

    [print] prints simplifications.

    Only one pass is performed,
    that is if clauses are simplified to unit clauses,
    these new unit clauses are not used to simplify already processed clauses.

    returns the simplified clause set,
    and the clauses and literals removed from the original clause set.

    @raise EMPTY_CLAUSE if a clause resolved to the empty clause.
*)
val simplify: print:bool -> clause list -> (clause list * clause list)
