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


(** term unification

  This modules provides the basic unification functions for terms and literals.

  For an explanation of {e offsets}, {e p_preserving}, {e recompute} see {!Subst}.*)

type term = Term.term
type literal = Term.literal
type clause = Term.clause
type binding = Subst.binding
type subst = Subst.subst


(** raised by all functions when the unification fails. *)
exception UNIFICATION_FAIL (*of subst*)




(** {6 Functions} *)


(** {2 Matching} *)

(** is the first term a (proper or improper) generalization of the second? *)
val is_term_generalization: ?p_preserving:bool -> term -> term -> bool

(** see {!Unification.is_term_generalization}. *)
val is_literal_generalization: ?p_preserving:bool -> literal -> literal -> bool

(** is the first term a (proper or improper) instance of the second? *)
val is_term_instance: ?p_preserving:bool -> term -> term -> bool

(** see {!Unification.is_term_instance}. *)
val is_literal_instance: ?p_preserving:bool -> literal -> literal -> bool

(** [match_terms term1 offset1 term2 offset2]
  returns a most general substitution which applied to [term1 offset1]
  produces [term2].

  {b raises} {!Unification.UNIFICATION_FAIL} *)
val match_terms: recompute:bool -> ?p_preserving:bool -> term -> int -> term -> int -> subst

(** like {!Unification.match_terms} over literals. *)
val match_literals: recompute:bool -> ?p_preserving:bool -> literal -> int -> literal -> int -> subst





(** {2 Unification} *)

(** are the two terms unifiable? *)
val are_terms_unifiable: ?p_preserving:bool -> term -> term -> bool

(** [unify_terms term1 offset1 term2 offset2]
  returns a most general substitution which applied to [term1 offset1]
  or [term2 offset2] produces the same term.

  prefers to bind universal variables to parametric ones
  instead of vice versa (see {!Admissible}).
  
  {b raises} {!Unification.UNIFICATION_FAIL} if no unifier exists. *)
val unify_terms: recompute:bool -> ?p_preserving:bool -> term -> int -> term -> int -> subst

(** like [unify_terms], but extends the given substitution. *)
val unify_terms_: recompute:bool -> ?p_preserving:bool -> subst -> term -> int -> term -> int -> subst


(** like {!Unification.unify_terms} over literals. *)
val unify_literals: recompute:bool -> ?p_preserving:bool -> literal -> int -> literal -> int -> subst

(** like [unify_literals], but extends the given substitution. *)
val unify_literals_: recompute:bool -> ?p_preserving:bool -> subst -> literal -> int -> literal -> int -> subst

(** unifies constraints of universal literals with offsets.
    e.g. the constraint [(p(x), 1), (q(f(x)), 2)] yields [\[ 1:x -> 2:f(x) \]] *)
val unify_constraints: recompute:bool -> ((literal * int) * (literal * int)) list -> subst

(** like {Unification.unify_constraints}, but returns a [hash_subst]. *)
val unify_constraints': recompute:bool -> ((literal * int) * (literal * int)) list -> Subst.hash_subst


(** {2 Substitution Merging} *)

(** Merging two substitutions into one substitution.

  Allows to unifiy two literal lists (or clauses)
  separately literal pair by literal pair
  and afterwords combining the computed substitution,
  instead of incrementally computing one substitution.

  Also used in substitution trees.

  Faster if the first substitution is the larger substitution. *)

(** [match_substs subsuming_offset subst1 subst2]
  merges [subst1] and [subst2].

  Terms with [subsuming_offset] have been matched to other terms,
  and the resulting substitutions are now to be merged.

  If a variable is bound to different terms in subst1 and subst2
  the bound terms are unified.
  I.e. if
  - both terms have [subsuming_offset] they are unified ({!Unification.unify_terms})
  - if one term has [subsuming_offset] it is matched
    ({!Unification.match_terms}) to the other
  - if no term has [subsuming_offset] the substitutions are not matcheable
    and {!Unification.UNIFICATION_FAIL} is raised.
*)
val match_substs: recompute:bool -> ?p_preserving:bool -> int -> subst -> subst -> subst

(** [unify_substs_offset subst1 subst2]
  merges [subst1] and [subst2].

  If a variable is bound to different terms in subst1 and subst2
  the bound terms are unified ({!Unification.unify_terms}).

  if [x -> y] and [y -> x] have to be merged one is dropped
  and the other kept, i.e. this is not considered to be a cyclic binding.

  Examples:
  - [ x -> a] and [ x -> b ] are not mergeable.
  - [ x -> g(y) ] and [ y -> a ] becomes [ x -> g(y); y -> a].
  - [ x -> g(y) ] and [ x -> g(a) ] becomes [ x -> g(y); y -> a].
  - [ x -> y] and [ y -> x ] becomes [x -> y] or [y -> x].

   {b raises} {!Unification.UNIFICATION_FAIL} on not mergeable substitutions. *)
val unify_substs: recompute:bool -> ?p_preserving:bool -> subst -> subst -> subst

(** a pre-check to unify_subst which only unifies top symbols of terms.
    {b raises} {!Unification.UNIFICATION_FAIL} if that fails. *)
val unify_substs_shallow: subst -> subst -> unit

(** [merge_substs subst1 subst2]
    merges [subst1] and [subst2] by adding all bindings into one substitution.

    if a variable is bound in both substitutions,
    {!Unification.UNIFICATION_FAIL} is raised if it is bound to different terms,
    otherwise only one of the two identical bindings is kept.
    no other consistency checks are performed,
    that is for example cycles may be introduced.
*)
val merge_substs: subst -> subst -> subst



(** {2 Subsumption} *)

(** does the first clause subsume the second one?
    that is, exists a subsitution that applied to the second clause
    yields the first clause (if seen as sets)?

    e.g. [\{ p(a) \}] subsumes [\{ p(x), p(a) \}].

    this implies that the subsuming clause has at most as many literal
    as the subsumed clause.
    despite of this, no prefiltering is done at all,
    subsumption is strictly done in an exponential trial and error way.

    the clauses are subsumed to be universal,
    i.e. unification is not done p-preserving.
*)
val subsumes: clause -> clause -> bool



(** a clause is condensed if it subsumes no proper subset of itself,
    i.e. it contains no logical equivalent subset.
    put another way,
    the clause and its condensed version mutually subsume each other
    (the subset trivially subsumes the superset),
    and there is no subset of the condensed clause for which this is the case.

    e.g. [\{ p(x), p(a) \}] is condensed to [\{ p(a) \}].

    worst case complexity is a linear number of subsumption tests.
*)
val condense: clause -> clause
