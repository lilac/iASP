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


type literal = Term.literal
type problem = Problem.problem
type flags = Flags.flags


type config = {
  flags: flags;
  problem: problem;
  is_horn: bool;
  is_fof: bool;
  is_theorem: bool;
  mutable start_time: float;

  mutable resolve: bool;
  mutable subsume: bool;
  mutable compact: bool;
  mutable productivity: bool;
  mutable mixed_literals: bool;

  mutable finite_domain: bool;
  mutable finite_domain_functionality: bool;
  mutable lemma: Flags.opt_lemma;
  mutable lemma_min: int;
  mutable lemma_max: int;
  mutable lemma_uip: bool;
  mutable lemma_parametric_assert: int;

  mutable equality: Flags.opt_equality;
  mutable plus_v: bool;
  mutable backtracking: Flags.opt_backtracking;
  mutable iterative_deepening: Flags.opt_iterative_deepening;
  mutable deepening_bound: int;
  mutable max_deepening_bound: int;
  mutable lookahead_exceeding: bool;
  mutable restart: Flags.opt_restart;
  mutable jumping: bool;
  mutable neg_assert_candidates: Flags.opt_neg_assert_candidates;

  mutable time_out_CPU: float;
  mutable time_out_WC: float;

  mutable print_level: int;
  mutable print_configuration: bool;
  mutable print_equality_axioms: bool;
  mutable print_lemmas: bool;
  mutable print_finite_domain_axioms: bool;
  mutable print_finite_domain_problem: Flags.opt_print_fd_problem;
  mutable print_statistics: bool;
  mutable print_model_context: bool;
  mutable print_model_context_file: string;
  mutable print_model_DIG: bool;
  mutable print_model_DIG_file: string;
(*  mutable print_model_ARM: bool;*)
  mutable print_model_finite: bool;
  mutable print_model_tptp: bool;
  mutable print_model_tptp_file: string;
  mutable print_derivation_online: bool;
  (*
  mutable print_derivation: bool;
  mutable print_derivation_file: string;
  mutable print_derivation_pruned: bool;
  mutable print_derivation_pruned_file: string;
  mutable print_derivation_dot: string;
  mutable print_derivation_dot_pruned: string;
  *)
  mutable print_derivation_context_unifier: bool;
  mutable print_assert_candidates: bool;
  mutable print_split_candidates: bool;

  mutable problem_file_name: string;

  (* for debugging only.
     some Context functions like is_satisfied, to_DIG
     rely on indexing. *)
  mutable term_indexing: bool;
}


let problem config = config.problem
let start_time config = config.start_time
let is_horn config = config.is_horn && config.problem#isHorn
let is_fof config = config.is_fof
let is_theorem config = config.is_theorem

let resolve config = config.resolve
let subsume config = config.subsume
let compact config = config.compact
let productivity config = config.productivity
let mixed_literals config = config.mixed_literals
let finite_domain config = config.finite_domain
let finite_domain_functionality config = config.finite_domain_functionality
let lemma config = config.lemma
let lemma_min config = config.lemma_min
let lemma_max config = config.lemma_max
let lemma_uip config = config.lemma_uip
let lemma_parametric_assert config = config.lemma_parametric_assert

let equality config = config.equality
let plus_v config = config.plus_v

let default_v config =
  if config.plus_v then
    Term.plus_v
  else
    Term.minus_v

let backtracking config = config.backtracking
let iterative_deepening config = config.iterative_deepening
let deepening_bound config = config.deepening_bound
let max_deepening_bound config = config.max_deepening_bound
let lookahead_exceeding config = config.lookahead_exceeding
let restart config = config.restart
let jumping config = config.jumping
let neg_assert_candidates config = config.neg_assert_candidates

let time_out_CPU config = config.time_out_CPU
let time_out_WC config = config.time_out_CPU

let print_level config = config.print_level
let print_configuration config = config.print_configuration
let print_equality_axioms config = config.print_equality_axioms
let print_lemmas config = config.print_lemmas
let print_finite_domain_axioms config = config.print_finite_domain_axioms
let print_finite_domain_problem config = config.print_finite_domain_problem
let print_statistics config = config.print_statistics
let print_model_context config = config.print_model_context
let print_model_context_file config = config.print_model_context_file
let print_model_DIG config = config.print_model_DIG
let print_model_DIG_file config = config.print_model_DIG_file
(*let print_model_ARM config = config.print_model_ARM*)
let print_model_finite config = config.print_model_finite
let print_model_tptp config = config.print_model_tptp
let print_model_tptp_file config = config.print_model_tptp_file
let print_derivation_online config = config.print_derivation_online
(*
let print_derivation config = config.print_derivation
let print_derivation_file config = config.print_derivation_file
let print_derivation_pruned config = config.print_derivation_pruned
let print_derivation_pruned_file config = config.print_derivation_pruned_file
let print_derivation_dot config = config.print_derivation_dot
let print_derivation_dot_pruned config = config.print_derivation_dot_pruned
*)
let print_derivation_context_unifier config = config.print_derivation_context_unifier
let print_assert_candidates config = config.print_assert_candidates
let print_split_candidates config = config.print_split_candidates

let term_indexing config = config.term_indexing

let problem_file_name config = config.problem_file_name

let drop_exceeding_eagerly config =
  (not config.lookahead_exceeding)
  &&
  (
    match config.restart with
      | Flags.RS_Eager
      | Flags.RS_Lazy ->
	  true
	    
      | Flags.RS_Delayed ->
	  false
  )



let create ~(flags: flags) ~(problem: problem) ~(is_horn: bool) ~(start_time: float)
    ~(fof: bool) ~(theorem:bool) : config =
  (* some sanity checks and changes of default settings *)
  let silent =
    (Flags.print_level flags)#value = 0
  in

  (* Horn *)
  if is_horn && problem#isHorn then begin
    (* horn always uses -v *)
    if (Flags.plus_v flags)#value then begin
      if not silent then
	print_endline ("Horn problem: using -v instead of +v");
      (Flags.plus_v flags)#set false;
    end;

    (* negative assert candidates can only be treated specially in the Horn case. *)
    if (Flags.neg_assert_candidates flags)#is_default
      &&
      (Flags.neg_assert_candidates flags)#value != Flags.NAC_Ignore
    then begin
      if not silent then
	print_endline ("Horn problem: ignoring negative assert candidates.");
      (Flags.neg_assert_candidates flags)#set_opt Flags.NAC_Ignore;
    end;
  end;

  let get_value get =
    (get flags)#value
  in
  let config =
    {
      start_time = start_time;
      problem = problem;
      is_horn = is_horn;
      is_fof = fof;
      is_theorem = theorem;
      flags = flags;
      resolve = get_value Flags.resolve;
      subsume = get_value Flags.subsume;
      compact = get_value Flags.compact;
      productivity = get_value Flags.productivity;
      mixed_literals = get_value Flags.mixed_literals;
      finite_domain = get_value Flags.finite_domain;
      finite_domain_functionality = get_value Flags.finite_domain_functionality;
      lemma = get_value Flags.lemma;
      lemma_min = get_value Flags.lemma_min;
      lemma_max = get_value Flags.lemma_max;
      lemma_uip = get_value Flags.lemma_uip;
      lemma_parametric_assert = get_value Flags.lemma_parametric_assert;
      
      equality = get_value Flags.equality;
      plus_v = get_value Flags.plus_v;
      backtracking = get_value Flags.backtracking;
      iterative_deepening = get_value Flags.iterative_deepening;
      deepening_bound = get_value Flags.deepening_bound;
      max_deepening_bound = get_value Flags.max_deepening_bound;
      lookahead_exceeding = get_value Flags.lookahead_exceeding;
      restart = get_value Flags.restart;
      jumping = false(*get_value Flags.jumping*);
      neg_assert_candidates = get_value Flags.neg_assert_candidates;

      time_out_CPU = get_value Flags.time_out_CPU;
      time_out_WC = get_value Flags.time_out_WC;

      print_level = get_value Flags.print_level;
      print_configuration = get_value Flags.print_configuration;
      print_equality_axioms = get_value Flags.print_equality_axioms;
      print_lemmas = get_value Flags.print_lemmas;
      print_finite_domain_axioms = get_value Flags.print_finite_domain_axioms;
      print_finite_domain_problem = get_value Flags.print_finite_domain_problem;
      print_statistics = get_value Flags.print_statistics;
      print_model_context = get_value Flags.print_model_context;
      print_model_context_file = get_value Flags.print_model_context_file;
      print_model_DIG = get_value Flags.print_model_DIG;
      print_model_DIG_file = get_value Flags.print_model_DIG_file;
(*      print_model_ARM = (* :TODO: get_value Flags.print_model_ARM*) false;*)
      print_model_finite = get_value Flags.print_model_finite;
      print_model_tptp = get_value Flags.print_model_tptp;
      print_model_tptp_file = get_value Flags.print_model_tptp_file;
      print_derivation_online = get_value Flags.print_derivation_online;
      (*
      print_derivation = get_value Flags.print_derivation;
      print_derivation_file = get_value Flags.print_derivation_file;
      print_derivation_pruned = get_value Flags.print_derivation_pruned;
      print_derivation_pruned_file = get_value Flags.print_derivation_pruned_file;
      print_derivation_dot = get_value Flags.print_derivation_dot;
      print_derivation_dot_pruned = get_value Flags.print_derivation_dot_pruned;
      *)
      print_derivation_context_unifier = get_value Flags.print_derivation_context_unifier;
      print_assert_candidates = get_value Flags.print_assert_candidates;
      print_split_candidates = get_value Flags.print_split_candidates;

      problem_file_name = Flags.problem_file_name flags;
      
      term_indexing = true;
    }
  in

    config








let print (config: config) =
  let print_flag = {
    Flags.apply =
      fun flag ->
	(* ignore unit options *)
	match flag#opt_to_string with
	  | [] ->
	      ()
		
	  | _ ->
	      Print.print_label flag#long_name (flag#value_to_string flag#value)
  }
  in
    print_endline ("Configuration:");
    List.iter
      (fun flag ->
	 Flags.process_flag flag print_flag
      )
      config.flags.Flags.flags;
    print_newline ();

    print_endline ("Problem:");
    Print.print_label "Horn" (string_of_bool config.problem#isHorn);
    Print.print_label "BS" (string_of_bool config.problem#isBS);
    Print.print_label "Equality" (string_of_bool config.problem#containsEquality);
    print_newline ()
