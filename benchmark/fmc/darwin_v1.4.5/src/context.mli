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


(** context with operations

    Internally applies the {e Compact} rule of the model evolution calculus.

    As only order preserving (chronological, backjumping) backtracking is used,
    the context can be seen as a stack of literals,
    backtracking always removes the last elements that were added.
    
    Each context literal has its unique non-negative id,
    which is strictly increasing within the literal stack,
    i.e. the oldest context literal has the lowest id,
    and the most recent added context literal has the highest id.

    If {!Const.ignore_skolem_literals} is true
    then skolem constants are not stored in the context
    but separately.
*)

(** {6 Types} *)

type config = Config.config
type statistic = Statistic.statistic
type counter = Counter.counter
type var = Var.var
type symbol = Symbol.symbol
type term = Term.term
type literal = Term.literal
type clause = Term.clause
type choice_point = State.choice_point
type state = State.state
type problem = Problem.problem
type bound = Bound.bound
type finite_domain = Finite_domain.finite_domain


(** a context literal with some information *)
type element = private {
  el_id: int;
  (** unique id per context literal. *)

  el_literal: literal;
  (** the context literal. *)

  el_choice_point: choice_point;
  (** the choice point in which the context literal was added. *)

  el_pars: var list;
  (** the parameters of the context literal. *)

  el_skolemized: bool;
  (** this literal contains skolem constants *)

  el_generation: int;
  (** depth of the tree of context literals
      needed to compute this context literal. *)

  el_is_fd_incomplete: bool;
  (** {!Config.finite_domain} mode is active,
      and iff true then this context literal depends on the totality axioms.
      That is, a refutation depends on the domain size,
      and is not a refutation of the original problem.
  *)

  mutable el_compacted: int;
  (** compacted by the context literal with the given id,
      or none if id = -1 *)
}


type context



(** {6 Functions} *)




(** {2 Initialization} *)

(** creates the context and fixes the indexing method based. *)
val create: config -> statistic -> state -> context


(** [from_file file_name]
  creates a context from a file.
  The file should contain one literal per line, readable by {!Read_tme}.

  @raise Sys_error on file access failure.

  @raise Failure if the file contains invalid input. *)
val from_file: string ->
  config -> statistic -> state -> context




(** {2 Constants} *)

(** for initialization usage, invalid choice point. *)
val null_element: element

(** pseudo assert literal for use as the assert gap in a context unifier,
    contains {!Term.assert_literal}, always valid choice point. *)
val assert_element: element

(** pseudo plus_v literal for use in a context unifier,
    contains {!Term.plus_v}, always valid choice point. *)
val plus_v_element: element

(** pseudo minus_v literal for use in a context unifier,
    contains {!Term.minus_v}, always valid choice point. *)
val minus_v_element: element


(** {2 Access} *)

(** [add context literal generation is_fd_incomplete]
    adds a new [literal] to the [context],
    for [generation] see {!Context.element}.
*)
val add: context -> literal -> int -> bool -> element

(** does the context contain only universal (i.e. no parametric) literals? *)
val is_universal: context -> bool

(** the same element? *)
val element_equal: element -> element -> bool


(** returns the element for a literal,
    if the literal is in the context. *)
val element_for_literal: context -> literal -> element option

(** returns the most recently added context literal.

    @raise Not_found if the context is empty. *)
val most_recent_element: context -> element

(** returns true if the first element has been
    added later to the context. *)
val is_more_recent: element -> element -> bool



(** {2 Iteration} *)

(** iter over all context elements *)
val iter: (element -> unit) -> context -> unit

(** fold over all context elements *)
val fold: ('a -> element -> 'a) -> 'a -> context -> 'a

(** iterover the compacted context elements *)
val iter_compacted: (element -> unit) -> context -> unit

(** fold over the compacted context elements *)
val fold_compacted: ('a -> element -> 'a) -> 'a -> context -> 'a


(** {2 Checks} *)

(** [check_contradictory context literal]
    checks if [literal] is contradictory with the [context].
    Returns a contradictory context element, if one exists.

    does also check skolem context literals
    even if {!Const.ignore_skolem_literals} is true.
*)
val check_contradictory: context -> literal -> element option

(** if {!Const.ignore_skolem_literals} then this
    returns true iff the literal is contradictory
    with a skolem literal in the context.
    
    otherwise this fails with an exception.
*)
val check_skolem_contradictory: context -> literal -> bool

(** [check_subsumed context literal]
    checks if [literal] is (p-preservingly) subsumed by the [context].
    Returns a subsuming context element, if one exists.

    does also check skolem context literals
    even if {!Const.ignore_skolem_literals} is true.
*)
val check_subsumed: context -> literal -> element option

(** [check_productive context producer produced]
    checks if [producer] is producing the negation of [produced] in the [context]
    (that is [producer] is the context literal and [produced] the remainder literal
    of the context unifier to check).
    Returns a shielding context element, if one exists. *)
val check_productive: context -> literal -> literal -> element option

(** is the ground literal true in the current context? *)
val is_true: context -> literal -> bool

(** [find_all_unifiable context literal]
    returns all (except for compacted) context elements that unify with [literal] *)
val find_all_unifiable: context -> literal -> element list

(** [find_all_subsuming context literal]
    returns all (except for compacted) context elements that (p-preservingly) subsume [literal] *)
val find_all_subsuming: context -> literal -> element list

(** {2 Representation} *)

val element_to_string: element -> string


(** prints the (not compacted) context literals, one literal per line. *)
val print_context: context -> out_channel -> unit

(** [print_DIG context out problem]
    prints the context in the DIG model representation,
    i.e. as a disjunction of implicit generalizations.
    an implicit generalization is an atom paired with a set of exception atoms,
    [A \ {B_0, ..., B_n} ],
    with the semantics that all ground instances of [A]
    except for all ground instances of any [B_i] are true in the described model.

    [problem] must contain the input problem.

    does only print non-compacted literals defined over the input signature,
    like {!Context.print_context} [show_model].
*)
val print_DIG: context -> out_channel -> problem -> unit


(** print the model represented by the context.

    in finite domain mode the finite domain must be given,
    and the bound is needed to get the current domain size. *)
val print_tptp_model: context -> out_channel -> problem -> finite_domain option -> bound -> unit

(** print the multiplication tables of a finite domain model.

    if [tptp] is true, then the
    {{:http://www.cs.miami.edu/~tptp/TSTP/FiniteInterpretations.html}TPTP format} is used,
    otherwise a more concise and easier human-readable notation.
*)
val print_multiplication_tables: context -> out_channel -> problem -> bound -> unit


(** string representation of the maximimal context size
    during the derivation (ala {!Print.print_label}). *)
val print_max_size: context -> unit



(** {2 Backtracking} *)

(** removes context elements added in invalid choice points,
    and undoes the corresponding compacting of the context.

    {b DEPENDENCY}: {!State_backtrack} must have been executed before.
*)
val backtrack: context -> unit

val contains: context -> element -> bool
