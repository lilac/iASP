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

(** lemma learning

    see the paper
    Lemma Learning in the Model Evolution Calculus
    by
    Peter Baumgartner, Alexander Fuchs, Cesare Tinelli.

    common functionality used by the lemma learning modules
    ({!Lemma_lifted}, {!Lemma_grounded}).
    
    These implement the computation of a lemma,
    i.e. a clause entailed by the problem clause set,
    from a closing context unifier.
    In essense this is done by resolution,
    starting with the closing clause,
    regressing over context literals used to close the clause,
    and resolving with the clauses on which the context literals were asserted.
    This is done recursively,
    till all assert literals are regressed,
    or some other termination criterion is met
    (like the unique implication point - UIP).
*)


(** {6 Types} *)

type literal = Term.literal
type clause = Term.clause

exception TAUTOLOGY

(** {6 Functions} *)

(** pseudo-context literal to regress,
    used to mark the regression task of the closing clause. *)
val close_literal: literal

(** accepts a list of literal lists,
    where each literal is the class of all regressed instances of the same context literal.

    simplifies by trying to unify and reduce all instances of a class to a singleton. *)
val simplify: literal list list -> literal list

(** removes permanently falsified equalities, e.g. 1 = 2.
    should only be used in finite domain mode.

    raises TAUTOLOGY if it contains a permanently satisfied equality like 1 = 1 or x = x.
    this can happen in the simplification of lifted lemma. *)
val remove_falsified_constraints: clause -> clause

(** abstracts permutable elements in finite domain mode
    (see {!Const.fd_isomorphism_abstraction}. *)
val abstract_permutable: clause -> Sort_inference.sorts -> clause
