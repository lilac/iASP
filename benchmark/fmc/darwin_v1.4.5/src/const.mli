(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2005, 2006
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


(** global constants

    basically, these values were determined by experiments
    and not considered important enough
    to be moved to the configuration options.
*)




(** {2 Exceptions} *)


(** [FOF theorem] is raised if the input is in fof format.
    [theorem] is true iff the input contains a conjecture. *)
exception FOF of bool

(** clausfier ran out of resources. *)
exception CLAUSIFIER_RESOURCE_OUT

(** the derivation is aborted because of a limitation of the implementation. *)
exception NO_SOLUTION of string

(** error in the input file. *)
exception PARSE_ERROR



(** {2 Debug} *)

(** compile the (slow) debug version which performs additional consistency checks,
    i.e. each module may perform additional consistency checks. *)
val debug: bool


(** ensures that independently of all flags the same derivation is produced.
    disables some optimizations. *)
val stable_derivation: bool


(** {2 Version} *)

(** the current system version, e.g. "Darwin 1.0" *)
val version: string


(** {2 Preprocessing Resolution} *)

(** the maximum length of a resolvent computed by {!Preprocessing_resolution.compute_resolvents}. *)
val resolvent_max_size: int

(** the maximum number of resolvents computed by {!Preprocessing_resolution.compute_resolvents}. *)
val resolvents_max_number: int


(** {2 Partial Context Unifiers} *)

(** how many partial context unifiers are explicitely cached in
    {!Context_unifier.context_partner}[.cp_partial_context_unifier]?
*)
val max_cached_partial_context_unifiers: int


(** {2 Candidates} *)

(** how many assert candidates (context unifiers) may be stored
    after being computed before being evaluated?
    might save on computation if a closing context unifier
    is found before the candidates need to be evaluated. *)
val max_unprocessed_assert_candidates: int

(** how many evaluated assert candidates (context unifiers) may be stored?
    this is the size of a cache of best candidates,
    worse candidates are forgotten until needed, then recomputed.

    this trades off potential recomputation time
    for a fixed upper limit on candidate memory usage. *)
val max_assert_candidates: int

(** how many assert candidates (within the current bound)
    should be stored in the lookahead? *)
val max_assert_lookahead: int

(** how many assert candidates exceeding the current bound
    should be stored in the lookahead? *)
val max_assert_lookahead_exceeding: int

(** like {!Const.max_unprocessed_assert_candidates} for
    split candidates. *)
val max_unprocessed_split_candidates: int


(** {2 Clause Utility} *)

(** decrease the utility value of clauses and lemmas
    after each [decay_clause_utility] backtrack operations. *)
val decay_clause_utility_interval: int

(** divide the clause utility by [decay_clause_utility_ratio] on decay. *)
val decay_clause_utility_ratio: int




(** {2 Lemmas} *)

(** computation of lemmas from conflict sets ({!Lemma}). *)


(** for lifted lemmas stop the computation of a lemma
    if more then [lemma_max_constraints] constraints are generated.
    just too expensive in general. *)
val lemma_max_constraints: int



(** {2 Restart with Jumping} *)

(** {!Jumping} through the search space. *)


(** a jump must at least skip over this many choice points. *)
val jumping_min_distance: int

(** Check after each [jump_check_every_splits] if a jump should be done
    (also based on the used time and the time limit). *)
val jumping_check_every_splits: int

(** for purposes of determining if a jump should be done
    the remaining time ration is assumed to be this fraction
    higher than it actually is (based on the given timeout and the used time). *)
val jumping_time_delta: float



(** {2 Finite Models} *)


(** use static symmetry reduction for constants

    for each sort do some static symmetry reduction for constants.

    E.g., if the constants are a, b, c, d, and the domain size is 3, then the axioms are:
    - a = 1
    - b = 1 \/ b = 2
    - c = 1 \/ c = 2 \/ c = 3
    - d = 1 \/ d = 2 \/ d = 3 \/ more

*)
val fd_static_symmetry_reduction: bool

(** use static symmetry reduction for unary functions

    in addition to symmetry reduction for constants:
    if there exists for a sort
    - a unary function f,
    - and there are k constans
    then do, for k=2 and domain size = 3:
    - a = 1
    - b = 1 \/ b = 2
    - f(1) = 1 \/ f(1) = 2 \/ f(1) = 3
*)
val fd_static_symmetry_reduction_unary: bool


(** compute the largest clique of disequalities,
    and set the initial domain size to its size. *)
val fd_compute_cliques: bool

(** use canonicity axioms in conjunction with static symmetry reduction.

    E.g., if the constants are a, b, c, d, and the domain size is 3, then the axioms are:
    c = 3 -> b = 2
    d = 3 -> b = 2 \/ c = 2
 *)
val fd_use_canonicity: bool

(** abstract permutable elements in lemmas,
    in order to prune isomorphic models.

    E.g., the lemma
    p(1, 2), q(2, 3)

    can be abstracted to the more general lemma
    p(x1, x2), q(x2, x3)

    to which one also needs to add the condition
    that all abstracted variables are pairwise distinct, i.e.:
    p(x1, x2), q(x2, x3) <- x1 != x2, x1 != x3, x2 != x3

    This is done separately for each sort.

    Note that this can not be done for elements
    for which static symmetry reduction is used,
    e.g. if we have the symmetry breaking axiom
    a = 1
    then 1 can not be abstracted anymore.

    To prevent a generalized lemma to be instantiated
    to non-permutable domain elements,
    the abstraction variables have to be guarded:
    p(1, x2), q(x2, x3) <- x2 != x3, perm(x2), perm(x3)

    and the following permuation axioms have to be added to the clause set:
    -perm
    perm(2)
    perm(3)


    Note that all of this is to be done separately for each sort,
    so separate perm axioms exist for each sort.
*)
val fd_isomorphism_abstraction: bool

(** use term definitions for flattening of deep terms.

    for example,

    p(f(g(x), y))

    will be transformed into the two clauses

    p(s0(x))

    and

    s0(x, y) = f(g(x), y),
    where s0 is a fresh skolem symbol.

    These are then flattened as usual.
    The definition term s0(x, y) is reused whenever
    an instance of f(g(x), y) has to be flattened.
*)
val fd_use_term_definitions: bool

(** add definitions for equalities.
    
    for example,
    after f(a, x) is flattened to
    x = y <- r_f(z, x), r_a(z)
    it produces the definition
    f(z, y) <- a(z).
*)
val fd_use_definitions: bool


(** replace {!Symbol.equality} by -{!Symbol.diff}.
    as except for the equality axioms
    all clauses contain only positive equalities,
    this has the effect that -v unifies less often,
    and there are less context unifiers in most cases. *)
val fd_use_diff: bool

(** instantiate the totality and functionality axioms for arities
    for which only 1 symbol exist.

    For example, if f is the only symbol of arity 1,
    then add the totality axiom
    r_2(f, x, 1) \/ ... \/ r_2(f, x, n)
    instead of the more general
    r_2(z, x, 1) \/ ... \/ r_2(z, x, n)
*)
val fd_instantiate_axioms: bool

(** don't handle equality and permutation restrictions
    (Const.fd_isomorphism_abstraction)
    axiomatically, but by using an internal constraint solver
    for equalities over finite domains *)
val fd_constraint_solver: bool

(** do a parametric right split even if the left split was universal.
    e.g. for p(x) the right split will be -p(u). *)
val fd_right_split_parametric: bool


(** use skolem literals only for close,
    but not for assert or split context unifiers.

    effects not only the finite domain but the general darwin mode.
*)
val ignore_skolem_literals: bool



(** {2 Representation} *)

(** string representation of the consts (see {!Print.print_label}). *)
val print: unit -> unit
