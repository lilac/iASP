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

(** finite domain model finding

    see the paper
    Computing Finite Models by Reduction to Function-Free Clause Logic
    by
    Peter Baumgartner, Alexander Fuchs, Hans de Neville, Cesare Tinelli.
*)


(** {6 Types} *)

type bound = Bound.bound
type symbol = Symbol.symbol
type term = Term.term
type clause = Term.clause
type problem = Problem.problem
type arities = Problem.arities
type sorts = Sort_inference.sorts

(** finite domain data type *)
type finite_domain



(** {6 Functions} *)


(** [create print problem domain_size]
    creates a finite domain representation based on [problem],
    and infers the problem sorts.

    prints the flattened clause set if [print_transformation] is true.

    prints the sorts if [print_sorts] is true.
*)
val create: print_transformation:bool -> print_sorts:bool -> problem -> bound option ->
  bool -> finite_domain

(** sets the bound, which enables [get_domain_size] *)
val set_bound: finite_domain -> bound -> unit

(** returns the original problem. *)
val get_problem: finite_domain -> problem

(** returns the flattened problem. *)
val get_flattened: finite_domain -> problem

(** returns the sorts of the problem. *)
val get_sorts: finite_domain -> sorts


(** returns the axioms for the given domain size,
    that is for totality and equality.

    if print is given the axioms are printed.

    returns all axioms, not only the new ones for this domain size.

    adds functionality axioms if [use_functionality_axioms] is true.
*)
val get_axioms: print:bool -> print_tptp:bool
  -> use_functionality_axioms:bool -> finite_domain -> int -> clause list


(** returns the i.th domain element. *)
val get_domain_element: int -> Term.term

(** returns all domain elements for the given domain size. *)
val get_domain_elements: int -> Term.term list

(** returns i, iff this is the i.th domain element.
    fails if it is not a domain element. *)
val get_id_of_domain_element: Term.term -> int


(** return the current domain size.
    may only be called if bound is given, either in [create] or with [set_bound]. *)
val get_domain_size: finite_domain -> int


(** transforms a relational term into all the functional terms represented by it.
    E.g., 
    r_2(z, x, y, 1)
    --> with the 2-ary function symbols f and g:
    f(x, y) = 1
    g(x, y) = 1
*)
val relation_to_equations: finite_domain -> term -> term list

(** replace = by -diff if Const.fd_use_diff is true. *)
val to_diff_literal: Term.literal -> Term.literal
