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


(** configuration for a derivation
*)




(** {6 Types} *)

type literal = Term.literal
type flags = Flags.flags
type problem = Problem.problem

type config





(** {6 Functions} *)


(** {2 Creation} *)

(** [create ~flags ~horn ~equality ~start_time ~fof ~theorem]
    creates a new configuration initialized with [flags].
    The problem is considered to be Horn, if [is-horn] is true,
    to contain equality predicates, if [contains_equality] is true.
    The time (wall clock) when the derivation was started is given with [start_time]
    [fof] states that the problem is formulated with first-order formulas,
    [theorem] that it also contains a conjecture.
*)
val create: flags:flags -> problem:problem -> is_horn:bool -> start_time:float -> fof:bool -> theorem:bool -> config

(** returns the (wall clock) time when the derivation was started. *)
val start_time : config -> float

(** is the problem (after transformations) horn? *)
val is_horn : config -> bool

(** is the original problem fof or cnf? *)
val is_fof : config -> bool

(** for a fof problems, does it contain a conjecture? *)
val is_theorem : config -> bool

(** returns the problem clause set to solve. *)
val problem : config -> problem

(** {2 Options} *)

(** For an explanation of most of these settings see also module [Flags]. *)

(** apply resolve. *)
val resolve : config -> bool

(** apply subsume. *)
val subsume : config -> bool

(** apply compact. *)
val compact : config -> bool

(** only consider productive remainders. *)
val productivity : config -> bool

(** allow context literals containing universal and parametric variables. *)
val mixed_literals : config -> bool

(** search for finite domain models. *)
val finite_domain : config -> bool


(** add functionality axioms for each function symbol in finite domain mode.

    e.g, for domain size 3 and the binary function symbol f add:

    f(x, y) != 1 \/ f(x, y) != 2

    f(x, y) != 2 \/ f(x, y) != 1

    f(x, y) != 1 \/ f(x, y) != 3

    f(x, y) != 3 \/ f(x, y) != 1

    f(x, y) != 2 \/ f(x, y) != 3

    f(x, y) != 3 \/ f(x, y) != 2

    which in essence is for all domain different elements d, d':

    f(x, y) = d => f(x, y) != d'
*)
val finite_domain_functionality: config -> bool

(** compute and store lemmas. *)
val lemma : config -> Flags.opt_lemma

(** when forgetting lemmas keep at least [lemma_min] lemmas. *)
val lemma_min : config -> int

(** forget lemmas when there are more than [lemma_max] lemmas. *)
val lemma_max : config -> int

(** use the UIP in lemma generation as described in {!Lemma}.  *)
val lemma_uip : config -> bool

(** apply parametric propagation on lemmas/clauses learnt at least n times. *)
val lemma_parametric_assert : config -> int

(** how should equality be handled? *)
val equality : config -> Flags.opt_equality

(** should {e +v} be used instead of {e -v} as the initial context literal? *)
val plus_v : config -> bool

(** returns {e +v} or {e -v}, depending on {!Config.plus_v}. *)
val default_v: config -> literal

(** the chosen backtracking method. *)
val backtracking : config -> Flags.opt_backtracking

(** the chosen iterative deepening method. *)
val iterative_deepening : config -> Flags.opt_iterative_deepening

(** the initial bound for iterative deepening.
    The current bound is maintained in {!Bound.bound}. *)
val deepening_bound : config -> int

(** the maximum bound for iterative deepening.
    if reached the prover terminates.
    if 0 then there is no maximum bound. *)
val max_deepening_bound : config -> int

(** apply the close look-ahaed also to Assert candidates exceeding the current deepening bound. *)
val lookahead_exceeding : config -> bool

(** the chosen restarting strategy. *)
val restart : config -> Flags.opt_restart

(** do jumps in the search space {!Jumping}. *)
val jumping : config -> bool

(** how to handle negative Assert candidate literals in the Horn case. *)
val neg_assert_candidates : config -> Flags.opt_neg_assert_candidates


(** should term indexing be applied for context checks? *)
val term_indexing : config -> bool

(** the file name of the problem. *)
val problem_file_name: config -> string


(** system cpu timeout in seconds. *)
val time_out_CPU: config -> float

(** system cpu timeout in seconds. *)
val time_out_WC: config -> float


(** {2 Verboseness} *)

(** print level. *)
val print_level : config -> int

(** print the configuration settings. *)
val print_configuration : config -> bool

(** print the added equality axioms, if any. *)
val print_equality_axioms : config -> bool

(** print the lemmas generated and kept over restarts. *)
val print_lemmas : config -> bool

(** print the axiomation in finite domain mode ({!Finite_domain}). *)
val print_finite_domain_axioms : config -> bool

(** print the generated problem in finite domain mode ({!Finite_domain}). *)
val print_finite_domain_problem : config -> Flags.opt_print_fd_problem

(** print a statistics after the derivation. *)
val print_statistics : config -> bool

(** print a found model as a context. *)
val print_model_context : config -> bool

(** like {!Config.print_model_context},
    but to the file with the given name. *)
val print_model_context_file : config -> string

(** print a found model as a DIG. *)
val print_model_DIG : config -> bool

(** like {!Config.print_model_DIG},
    but to the file with the given name. *)
val print_model_DIG_file : config -> string

(*(** print a found model as an ARM. *)
val print_model_ARM : config -> bool*)

(** print the multiplication tables of a found finite domain model. *)
val print_model_finite : config -> bool

(** print a model in
    {{:http://www.cs.miami.edu/~tptp/TSTP/FiniteInterpretations.html}TPTP format}
*)
val print_model_tptp : config -> bool

(** like {!Config.print_model_tptp},
    but to the file with the given name. *)
val print_model_tptp_file : config -> string

(** print Assert, Split, and Close applications when they are applied
  during the derivation,
  plus stuff like the depth bound, backtracking, and restarts. *)
val print_derivation_online : config -> bool
(*
(** like {!Config.print_derivation_online},
    but after the derivation is finished. *)
val print_derivation : config -> bool

(** like {!Config.print_derivation},
    but to the file with the given name. *)
val print_derivation_file : config -> string

(** like {!Config.print_derivation},
    but split decisions without any impact on the derivation are omitted. *)
val print_derivation_pruned : config -> bool

(** like {!Config.print_derivation_pruned},
    but to the file with the given name. *)
val print_derivation_pruned_file : config -> string

(** like {!Config.print_derivation_file},
    but in the
    {{:http://www.graphviz.org/cgi-bin/man?dot} graphviz dot format}.

    inference steps are represented by different node colors:
    - Split -> green
    - Assert -> blue
    - Unit Split -> yellow
    - Close -> red
    - Incomplete (kind of Close) -> maroon (red-brown)
*)
val print_derivation_dot : config -> string

(** like {!Config.print_derivation_dot},
    but split decisions without any impact on the derivation are omitted. *)
val print_derivation_dot_pruned : config -> string
*)
(** print the corresponding context unifier when an application
  of Assert, Split, or Close is printed. *)
val print_derivation_context_unifier : config -> bool

(** print each computed applicable assert candidate. *)
val print_assert_candidates : config -> bool

(** print each computed applicable split candidate. *)
val print_split_candidates : config -> bool



(** {2 Representation} *)

(** string representation of the configuration (see {!Print.print_label}). *)
val print: config -> unit
