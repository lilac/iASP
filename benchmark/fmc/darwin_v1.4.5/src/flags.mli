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

(** command line evaluation

  Evaluates the command line arguments so that it can be used by {!Config.create}.
  Prints a help message if requested.
*)



(** {6 Types} *)

(** a flag is identified by its id. *)
type flag_id



(** {2 [opt_xxx]} *)

(** The valid arguments for the flags which accept strings. *)


(** the valid arguments for the equality flag,
    i.e. how the predicate symbol {e =} should be interpreted. *)
type opt_equality =
  | EQ_None
      (** Do nothing, {e =} is uninterpreted. *)
  | EQ_Axioms
      (** add the axioms of equality to the input clause set. *)

(** the valid arguments for the backtracking flag. *)
type opt_backtracking =
  | BT_Naive
      (** perform naive backtracking,
	  i.e. build the full binary decision tree
	  without skipping any split decision on backtracking. *)
  | BT_Backjumping
      (** perform backjumping (dependency directed backtracking). *)

(** the valid arguments for the iterative deepening flag. *)
type opt_iterative_deepening =
  | IT_TermDepth
      (** perform iterative deeping over the term depth of candidate literals. *)
  | IT_TermWeight
      (** perform iterative deeping over the term weight of candidate literals. *)

(** the valid arguments for the restarting strategies flag.
    Restarting is done when no refutation is possible within the current depth bound,
    and no model has been found within the current bound {!Bound}. *)
type opt_restart =
  | RS_Eager
      (** candidates exceeding the current depth bound are dropped eagerly.
	  when an incomplete exhausted branch is found,
	  i.e. a branch that is only exhausted because candidate have been dropped,
	  the derivation is restarted with an increased depth bound. *)
  | RS_Lazy
      (** like [RS_Eager] exceeding candidates are dropped eagerly,
	  but when an incomplete branch is found,
	  restarting is not necessarily done.
	  Instead, the derivation is backtracked to a (left split) choice point
	  up to which no candidate has been dropped,
	  and the derivation is continued with the (right split) inverse choice
	  from there.
	  Thus, more search space branches are explored for a model,
	  but finding a refutation might be delayed.
      *)
  | RS_Delayed
      (** restarting is deferred like in [RS_Lazy],
	  but in addition all exceeding candidates are kept.
	  Only after an exhausted branch has been found,
	  it is checked for incompleteness with respect to
	  the kept exceeding candidates and the current context.
      *)


(** how to handle negative assert candidates. *)
type opt_neg_assert_candidates =
  | NAC_Ignore
      (** completely ignore them. *)
  | NAC_Lookahead
      (** use them only for the Close lookahead ({!Selection_assert}, {!Selection_lookahead}),
	  but don't apply {e Assert}. *)
  | NAC_Use
      (** apply {e Assert} to the negative candidates. *)

(** lemma computation. *)
type opt_lemma =
  | LM_None
      (** do not compute lemmas. *)
  | LM_Grounded
      (** ground closing clause, then start lemma computation. *)
  | LM_Lifted
      (** start lemma computation from closing clause. *)
  | LM_Propositional
      (** use propositional abstraction of context literals. *)

(** preprocessing of input clauses by splitting into smaller clauses. *)
type opt_preprocess_split =
  | PPS_None
      (** do nothing. *)
  | PPS_Ground
      (** split variable disjoint clause parts,
          i.e. the parts are connected by ground literals. *)
  | PPS_NonGround
      (** split also not variable disjoint clause parts,
          i.e. the parts are connected by non-ground literals. *)


(** the supported input formats. *)
type opt_input_format =
  | FI_TME
      (** tme ({!Read_tme}). *)
  | FI_TPTP
      (** tptp ({!Read_tptp}). *)
  | FI_Darwin
      (** darwin ({!Read_darwin}). *)

(** printing of finite domain problems for each domain size. *)
type opt_print_fd_problem =
  | PFD_Silent
      (** don't print *)
  | PFD_Print
      (** print *)
  | PFD_Exit
      (** print and exit *)

(** {2 Flags} *)


(** signature for all flags

    A flag represents a setting ({!Flags.flag_type.value}).
    Its input might ['b] be of a different type
    than the stored setting ['a],
    especially for the special flags which take strings as input,
    but store custom types internally ([opt_xxx ]).
 *)
class type ['a, 'b] flag_type =
object
  method id : flag_id
    (** the flag id. *)
  method short_name : string
    (** e.g. "-db". *)
  method long_name : string
    (** e.g. "--depth-bound". *)
  method description : string list
    (** e.g. "Initial depth bound.". *)
  method value : 'a
    (** e.g. 3. *)
  method is_default : bool
    (** false iff the value was specified by a command line flag *)

  method argument_to_string : 'b -> string
    (** string representation of an argument. *)
  method value_to_string : 'a -> string
    (** string representation of a value. *)
  method opt_to_string : string list
    (** string representation of the valid arguments. *)
  method signature : string
    (** the range of valid values. *)

  method set : 'b -> unit
    (** performs a validity check and updates the flag's value
	resp. @raise Arg.Bad exception on failure. *)

  method set_opt : 'a -> unit
    (** changes the value according to the argument. *)
end



(** the different flag types *)

type flag_type_bool = (bool, bool) flag_type
type flag_type_int = (int, int) flag_type
type flag_type_float = (float, float) flag_type
type flag_type_unit = (unit, unit) flag_type
type flag_type_string = (string, string) flag_type
type flag_type_equality = (opt_equality, string) flag_type
type flag_type_backtracking = (opt_backtracking, string) flag_type
type flag_type_iterative_deepening = (opt_iterative_deepening, string) flag_type
type flag_type_restart = (opt_restart, string) flag_type
type flag_type_neg_assert_candidates = (opt_neg_assert_candidates, string) flag_type
type flag_type_lemma = (opt_lemma, string) flag_type
type flag_type_preprocess_split = (opt_preprocess_split, string) flag_type
type flag_type_input_format = (opt_input_format, string) flag_type
type flag_type_print_fd_problem = (opt_print_fd_problem, string) flag_type


(** unifies the different flag types. *)
type flag

(** the result of the evaluation of the command line. *)
type flags = private {
  flags: flag list;
  (** all flags updated according to the command line. *)
  mutable problem_file_name: string;
  (** the name of the file containing the problem description. *)
}



(** {6 Functions} *)


(** {2 Creation} *)

(** creates a configuration based on the command line options. *)
val eval_command_line: unit -> flags

(** [create file_name] creates a default configuration. *)
val create: string -> flags



(** {2 Access} *)

(** These access functions map straightforward to the corresponding ones in {!Config}. *)

val resolve : flags -> flag_type_bool
val subsume : flags -> flag_type_bool
val compact : flags -> flag_type_bool
val productivity : flags -> flag_type_bool
val mixed_literals : flags -> flag_type_bool
val finite_domain : flags -> flag_type_bool
val finite_domain_functionality : flags -> flag_type_bool
val unique_name_assumption : flags -> flag_type_bool
val lemma : flags -> flag_type_lemma
val lemma_min : flags -> flag_type_int
val lemma_max : flags -> flag_type_int
val lemma_uip : flags -> flag_type_bool
val lemma_parametric_assert : flags -> flag_type_int
val equality : flags -> flag_type_equality
val plus_v : flags -> flag_type_bool
val backtracking : flags -> flag_type_backtracking
val iterative_deepening : flags -> flag_type_iterative_deepening
val deepening_bound : flags -> flag_type_int
val max_deepening_bound : flags -> flag_type_int
val lookahead_exceeding : flags -> flag_type_bool
val restart : flags -> flag_type_restart
val jumping : flags -> flag_type_bool
val neg_assert_candidates : flags -> flag_type_neg_assert_candidates
val eprover : flags -> flag_type_string
val preprocess_split : flags -> flag_type_preprocess_split
val preprocess_unit : flags -> flag_type_bool
val preprocess_pure : flags -> flag_type_bool
val preprocess_resolution : flags -> flag_type_bool
val preprocess_equality : flags -> flag_type_bool
val preprocess_clausifier : flags -> flag_type_bool
val input_format: flags -> flag_type_input_format

val time_out_CPU : flags -> flag_type_float
val time_out_WC : flags -> flag_type_float
val memory_limit : flags -> flag_type_int

val print_level : flags -> flag_type_int
val print_preprocess_split : flags -> flag_type_bool
val print_preprocess_unit : flags -> flag_type_bool
val print_preprocess_pure : flags -> flag_type_bool
val print_preprocess_resolution : flags -> flag_type_bool
val print_preprocess_equality : flags -> flag_type_bool
val print_configuration : flags -> flag_type_bool
val print_lemmas : flags -> flag_type_bool
val print_finite_domain_sorts : flags -> flag_type_bool
val print_finite_domain_transformation : flags -> flag_type_bool
val print_finite_domain_axioms : flags -> flag_type_bool
val print_finite_domain_problem : flags -> flag_type_print_fd_problem
val print_equality_axioms : flags -> flag_type_bool
val print_statistics : flags -> flag_type_bool
val print_model_context : flags -> flag_type_bool
val print_model_context_file : flags -> flag_type_string
val print_model_DIG : flags -> flag_type_bool
val print_model_DIG_file : flags -> flag_type_string
val print_model_ARM : flags -> flag_type_bool
val print_model_finite : flags -> flag_type_bool
val print_model_tptp : flags -> flag_type_bool
val print_model_tptp_file : flags -> flag_type_string
val print_derivation_online : flags -> flag_type_bool
(*
val print_derivation : flags -> flag_type_bool
val print_derivation_file : flags -> flag_type_string
val print_derivation_pruned : flags -> flag_type_bool
val print_derivation_pruned_file : flags -> flag_type_string
val print_derivation_dot : flags -> flag_type_string
val print_derivation_dot_pruned : flags -> flag_type_string
*)
val print_derivation_context_unifier : flags -> flag_type_bool
val print_assert_candidates : flags -> flag_type_bool
val print_split_candidates : flags -> flag_type_bool

val problem_file_name: flags -> string

(* disable zip support *)
(** if zip support is enabled ({!Zip_wrapper.enabled}),
    and a zipped input file has been given,
    its name is provided here. *)
val zipped_file_name : flags -> string



(** {2 Flag Processing} *)

(** for reasons of the type system it is not possible to pass
    the function contained in this type directly to {!Flags.process_flag}.
    To prevent hat ['b 'c] are instantiated to the first tested flag subtypes stored in a flag
    this polymorphic record has to be used (at least as in OCaml 3.08).
    
    Example: The description of a flag is returned with
    
  [Flags.process_flag flag { Flags.apply = fun flag -> flag#description }]
    
*)
type 'a apply_to_flag = {
  apply : 'b 'c. ('b, 'c) flag_type -> 'a;
}

(** applies a function to the {!Flags.flag_type} subtype contained in the flag. *)
val process_flag: flag -> 'a apply_to_flag -> 'a
