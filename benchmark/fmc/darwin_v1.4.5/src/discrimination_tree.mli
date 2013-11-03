(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2004, 2005, 2006
              The University of Iowa
              Universitaet Koblenz-Landau 

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


(** term indexing with a discrimination tree
*)



(** {6 Types} *)


type term = Term.term
type literal = Term.literal
type clause = Term.clause
type 'a data = 'a Term_indexing.data
type 'a predicate_index = 'a Term_indexing.predicate_index
type 'a index = 'a Term_indexing.index






(** {6 Functions} *)

(** creates a {!Term_indexing.predicate_index} using discrimination trees. *)
val create_predicate_index: bool -> 'data data -> 'data predicate_index

(** [create_index productivity data]
    creates a {!Term_indexing.index} using discrimination trees.

    as a memory optimization productivity must be true
    if the checks related to productiviy checks are to be supported.
    otherwise those checks will fail
    ({!Term_indexing.predicate_index.get_shielding_iterator}.
    {!Term_indexing.predicate_index.find_all_shielding},
    {!Term_indexing.predicate_index.find_strongly_shielding},
    {!Term_indexing.predicate_index.find_shielding}).
*)
val create_index: bool -> 'data data -> 'data index


(** creates a term index specialized to contain ints as values.
*)
val create_int_index: bool -> int index


(** creates a term index specialized to contain terms as values.

    uses {!Term.term_equal} for comparison.
*)
val create_term_index: bool -> term index


(** creates a term index specialized to contain literals as values.

    uses {!Term.literal_equal} for comparison.
*)
val create_literal_index: bool -> literal index


(** creates a term index specialized to contain clauses as values.

    uses {!Term.clause_equal} for clause comparison.
*)
val create_clause_index: bool -> clause index


(** creates a term index specialized to contain (literal, clause) pairs as values.

    uses {!Term.literal_equal} and {!Term.clause_equal} for clause comparison.
*)
val create_literal_clause_index: bool -> (literal * clause) index
