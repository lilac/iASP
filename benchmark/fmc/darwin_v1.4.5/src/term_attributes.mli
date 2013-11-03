(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2004, 2005
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


(** computation and comparison of term attributes *)


(** {6 Types} *)

type term = Term.term
type literal = Term.literal
type clause = Term.clause
type subst = Subst.subst

(** a term is
    - {e universal}, if it contains no parameters,
    - {e mixed}, if it contains parameters and variables,
    - {e parametric}, if it contains parameters but no variables.
*)
type pureness


(** {6 Functions} *)


(** {2 Pureness } *)

(** pureness of a term. *)
val get_pureness: term -> pureness

(** is the term universal? *)
val is_universal: pureness -> bool

(** is the term mixed? *)
val is_mixed: pureness -> bool

(** is the term parametric? *)
val is_parametric: pureness -> bool

(** faster shortcut for {!Term_attributes.is_parametric} ({!Term_attributes.get_pureness} term). *)
val is_term_parametric: term -> bool

(** is the literal propositional, i.e. a predicate symbol without arguments? *)
val is_propositional: literal -> bool

(** [cmp_pureness_universality pureness1 pureness2] returns
    - -1, if pureness1 is universal but pureness2 is not,
    -  1, if pureness2 is universal but pureness1 is not,
    -  0, otherwise
*)
val cmp_pureness: pureness -> pureness -> int

(** [cmp_pureness_universality pureness1 pureness2]
    returns -1, 0, or 1 and prefers
    first, terms with as few parameters as possible,
    and second, terms with as much universal variables as possible.
*)
val cmp_universality: pureness -> pureness -> int



(** {2 Depth } *)

(** depth of a literal,
    i.e. the deepest branch if the term is seen as a tree.

    Examples:
    - a -> 0
    - f(a, b) -> 1
    - f(a, g(b)) -> 2
*)
val depth_of_literal : literal -> int

(** [depth_of_literal_subst subst literal offset]
  is like {!Term_attributes.depth_of_literal},
  but variables in [literal] are replaced by their bindings
  in [subst] *)
val depth_of_literal_subst : literal -> subst -> int -> int

(** [bounded_depth_of_literal literal bound] returns
    the depth of the literal, but limited to the given bound.
    That is, even if the real depth is higher,
    the search will be stopped as soon as the depth is >= bound.

    faster than using [depth_of_literal] for this,
    as the computation is stopped as soon the depth exceeds the bound. *)
val bounded_depth_of_literal : literal -> int -> int

(** like depth_of_literal_subst for bounded_depth_of_literal *)
val bounded_depth_of_literal_subst : literal -> subst -> int -> int -> int




(** {2 Weight } *)


(** weight of a literal,
    i.e. the number of function/constant symbol occurrences in the term.

    Examples:
    - a -> 1
    - f(a, a) -> 3
    - f(x, g(b)) -> 3
    - f(u, g(b)) -> 3
*)
val weight_of_literal: literal -> int

(** [weight_of_literal_subst subst literal offset]
  is like {!Term_attributes.weight_of_literal},
  but variables in [literal] are replaced by their bindings
  in [subst] *)
val weight_of_literal_subst : literal -> subst -> int -> int

(** weight of a clause, i.e. the sum of the literal weights. *)
val weight_of_clause: clause -> int

(** like [bounded_depth_of_literal] for weight *)
val bounded_weight_of_literal : literal -> int -> int

(** like [bounded_depth_of_literal_subst] for weight *)
val bounded_weight_of_literal_subst : literal -> subst -> int -> int -> int
