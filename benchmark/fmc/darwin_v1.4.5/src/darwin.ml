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


(* Sketch of the algorithm:
   1) select a new candidate
   2) add candidate to context
      - create a new choice point for a split candidate
   3) if this triggers a Close:
     - invalidate the most recent involved choice point
     - add its corresponding right split to the candidate set
     - remove all now invalidated information
   4) compute new assert and split candidates based on the new context literal
   5) continue with 1)
*)



(*** types ***)

type flags = Flags.flags
type config = Config.config
type bound = Bound.bound
type statistic = Statistic.statistic
type choice_point = State.choice_point
type symbol = Symbol.symbol
type literal = Term.literal
type clause = Term.clause
type problem = Problem.problem
type state = State.state
type jump = Jumping.jumping
type raw_context_unifier = Context_unifier.raw_context_unifier
type context = Context.context
type selection = Selection.selection
type selected = Selection_types.selected
type log = Log.log
type finite_domain = Finite_domain.finite_domain

(* (sorted) mapping from an arity to the input symbols with this arity *)
type arities = Problem.arities

type derivation = {
  (* setup *)
  dv_config: config;
  dv_bound: bound;
  dv_statistic: statistic;
  dv_state: state;
  dv_context: context;
  dv_selection: selection;
  dv_log: log;

  (* might contain same saved branches of previous incomplete derivations,
     which still have to be processed. *)
  dv_jump: jump;
  (* are we currently replaying a saved branch? *)
  dv_in_saved_branch: bool;

  (* the problem to solve *)
  dv_problem: problem;

  (* finite domain transformation *)
  dv_finite_domain: finite_domain option;
  (* is the current transformation an incomplete (partial) finite domain refutation. *)
  mutable dv_is_fd_incomplete: bool;
}





exception CLOSE = Context_unifier.CLOSE
exception SATISFIABLE = Selection.SATISFIABLE
exception UNSATISFIABLE = Context_unifier.UNSATISFIABLE
exception NO_SOLUTION = Const.NO_SOLUTION


(* raised if gc needs more memory than specified as the limit *)
exception OUT_OF_MEMORY

(* raised if max. depth / domain size is reached *)
exception BOUND_EXHAUSTED of derivation


(*** signal handling ***)

(* raised TERM_SIGNAL on termination signal *)
exception TERM_SIGNAL of string


(* return SZS result status, based on fof or cnf input *)
let get_SZS_unsatisfiable (fof: bool) =
  if fof then
    "Theorem"
  else
    "Unsatisfiable"

let get_SZS_satisfiable (fof: bool) =
  if fof then
    "CounterSatisfiable"
  else
    "Satisfiable"

let get_SZS_status_line (result: string) (problem: string) : string =
  "SZS status " ^ result ^ " for " ^ problem

let handle_term_signal (signal_number: int) : unit =
  if signal_number = Sys.sigint then
    raise (TERM_SIGNAL "\nABORTED interactive interrupt")

  else if signal_number = Sys.sigterm then
    raise (TERM_SIGNAL "\nABORTED termination")

  else if signal_number = Sys.sigalrm then
    raise (TERM_SIGNAL "\nSZS status Timeout")

  else if signal_number = Sys.sigvtalrm then
    raise (TERM_SIGNAL "\nSZS status Timeout")

  else
    raise (TERM_SIGNAL ("\nABORTED: " ^ string_of_int signal_number))


let init_signal_handler () =
  Sys.set_signal Sys.sigint (Sys.Signal_handle handle_term_signal);
  Sys.set_signal Sys.sigterm (Sys.Signal_handle handle_term_signal);
  Sys.set_signal Sys.sigsegv (Sys.Signal_handle handle_term_signal)



let handle_memory_limit (limit: int) () =
  let memory =
    (* words / (MBit / bits per word) *)
    ((Gc.stat ()).Gc.top_heap_words) / ((1024 * 1024 * 8) / Sys.word_size)
  in
    if memory < 0
      ||
      (limit > 0 && int_of_float ((float_of_int memory) *. 1.1) >= limit)
    then
      raise OUT_OF_MEMORY



(* set user defined timeouts *)
let set_time_outs (flags: flags) : unit =
  let set_time_out (time_out: float) (timer: Unix.interval_timer) signal : unit =
    if time_out > 0.0 then begin
      try
	Sys.set_signal signal (Sys.Signal_handle handle_term_signal);
	ignore (
	    Unix.setitimer timer
	      {
   		Unix.it_interval = 0.0;
   		Unix.it_value  = time_out;
	      }
	      : Unix.interval_timer_status);
      with
	| Invalid_argument argument ->
	    print_endline ("Couldn't set time out: " ^ argument);
	    print_endline ("SZS status GaveUp");
	    exit 0;
    end
  in
    set_time_out ((Flags.time_out_CPU flags)#value) Unix.ITIMER_VIRTUAL Sys.sigvtalrm;
    set_time_out ((Flags.time_out_WC flags)#value) Unix.ITIMER_REAL Sys.sigalrm








(* in finite derivation mode, does this context unifier depend on the current domain size?*)
let is_fd_incomplete (derivation: derivation) (raw_context_unifier: raw_context_unifier) : bool =
  (* finite domain mode *)
  Config.finite_domain derivation.dv_config
  &&
  (* dependend on the current domain size, i.e. the totality axioms. *)
  Tools.array_exists
    (fun context_partner -> context_partner.Context_unifier.cp_element.Context.el_is_fd_incomplete)
    raw_context_unifier.Context_unifier.rcu_context_partners





(*** add ***)

(* adds a new literal to the context and candidate set,
   this implicitely computes new context unifiers. *)
let add (derivation: derivation) (selected: selected) : unit =
  let is_fd_incomplete =
    (* based on domain size marker *)
    is_fd_incomplete derivation selected.Selection_types.raw_context_unifier
    ||
    (* or the domain size marker *)
    Term.is_fd_size_marker selected.Selection_types.literal
  in
  let context_element =
    Context.add
      derivation.dv_context
      selected.Selection_types.literal
      (Context_unifier.generation_of_candidate
	 selected.Selection_types.raw_context_unifier.Context_unifier.rcu_context_partners)
      is_fd_incomplete
  in
    Selection.add derivation.dv_selection context_element








(*** inference rules ***)


let assert_rule (derivation: derivation) (selected: selected) : unit =
  Statistic.inc_assert derivation.dv_statistic;

  (* assert *)
  if selected.Selection_types.candidate_type = State.Propagation then begin
    derivation.dv_log#apply_assert selected;
  end
    
  (* unit split, but treated like assert. *)
  else begin
(*    Statistic.inc_split derivation.dv_statistic;*)
    derivation.dv_log#apply_split_unit selected;
  end;

  add derivation selected

    
let split_left (derivation: derivation) (selected: selected) : unit =
  Statistic.inc_split derivation.dv_statistic;
  derivation.dv_log#apply_split_left
    (State.active_choice_point derivation.dv_state) selected;
  
  add derivation selected


let split_right (derivation: derivation) (selected: selected) : unit =
(*  Statistic.inc_split derivation.dv_statistic;*)
(*  Statistic.inc_assert derivation.dv_statistic;*)
  derivation.dv_log#apply_split_right selected;
  add derivation selected



let close (derivation: derivation) (raw_context_unifier: raw_context_unifier) : clause option =
  Statistic.inc_close derivation.dv_statistic;
  derivation.dv_log#close raw_context_unifier;

  (* check incompleteness in finite domain mode *)
  derivation.dv_is_fd_incomplete <- derivation.dv_is_fd_incomplete || is_fd_incomplete derivation raw_context_unifier;

  (* before backtracking all other modules the current state must be backtracked.
     this invalidates retracted states, and changes the current active state. *)
  let retracted, explanation, lemma =
    State_backtrack.backtrack
      derivation.dv_state derivation.dv_context derivation.dv_config raw_context_unifier
  in

    (* backtrack all modules *)
    derivation.dv_log#backtrack;
    derivation.dv_bound#backtrack;
    Context.backtrack derivation.dv_context;
    Selection.backtrack derivation.dv_selection retracted explanation;

    lemma


(* an exhausted branch has been found.
   now it remains to check,
   - if it is complete, i.e. _all_ candidates have been processed
     and we found a model,
   - or if it is incomplete.
     then, how to proceed depends on the restart strategy
*)
let check_exhausted (derivation: derivation) : clause option =
  (* no backtracking over incomplete branches,
     terminate derivation within this bound (restart might still be done). *)
  if Config.restart derivation.dv_config = Flags.RS_Eager then begin
    raise SATISFIABLE
  end

  (* abandon this branch and try to continue the derivation. *)
  else begin
    match derivation.dv_bound#dropped_choice_point with
      | None ->
	  (* a model has been found, terminate *)
	  raise SATISFIABLE

      | Some retracted ->
	  (* incomplete branch, try to backtrack and continue derivation
	     from a complete choice point. *)

	  derivation.dv_log#incomplete;

	  (* do an incomplete backtracking,
	     i.e. mark the current derivation as incomplete,
	     and handle the dependencies special. *)
	  derivation.dv_bound#incomplete;
	  State_backtrack.backtrack_incomplete derivation.dv_state retracted;

	  (* backtrack all modules *)
	  derivation.dv_log#backtrack;
	  derivation.dv_bound#backtrack;
	  Context.backtrack derivation.dv_context;
	  (* empty explanation for the backtracking.
	     this breaks backjumping, but we are already incomplete anyway *)
	  Selection.backtrack derivation.dv_selection retracted [];
	  None
  end



(*** selection ***)

(* endless loop:
   - select a candidate
   - add it to the context
   - backtrack on CLOSED and right split

   loop is exited when
   - UNSATISFIABLE is raised
   - SATISFIABLE is raised and restarting has to be done
*)
let select (derivation: derivation) : unit =
  let lemma = ref None in
  while true do
    try
      begin
	match !lemma with
	  | None -> ()

	  | Some clause ->
	      lemma := None;
	      Selection.add_lemma derivation.dv_selection clause;
      end;

      (* get next candidate *)
      let selected =
	Selection.select derivation.dv_selection
      in
	(* apply the rule *)
	match selected.Selection_types.candidate_type with
	  | State.Propagation ->
	      assert_rule derivation selected
		
	  | (*Selection_types*)State.SplitLeft ->
	      (* replaying a guiding path, so do the split *)
	      if derivation.dv_in_saved_branch then begin
		split_left derivation selected
	      end
		
	      else begin
		(* check if a jump should be done *)
		match Jumping.jump derivation.dv_jump with
		  | None ->
		      split_left derivation selected
			
		  | Some jump_target ->
		      (* jump *)
		      derivation.dv_log#jump;
		      State_backtrack.backtrack_incomplete derivation.dv_state jump_target;

		      derivation.dv_log#backtrack;
		      derivation.dv_bound#backtrack;
		      Context.backtrack derivation.dv_context;
		      (* full remaining branch as explanation for the backtracking.
			 this in essence disables backjumping,
			 but is necessary to retain completeness.
		         does not work with lemmas. *)
		      Selection.backtrack derivation.dv_selection jump_target
			(List.map (fun cp -> State.left_split_literal cp) (State.get_branch derivation.dv_state));
	      end

	  | (*Selection_types*)State.SplitRight ->
	      split_right derivation selected
    with
      | CLOSE raw_context_unifier ->
	  (* closed branch found *)

	  (* replaying of a guiding path failed? *)
	  if Selection.in_replay derivation.dv_selection then begin
	    failwith "Darwin.select: replay guiding path failed.";
	    (* if lemmas are used with guiding paths this is actually ok.
	       raise UNSATISFIABLE
	    *)
	  end;

	  lemma := close derivation raw_context_unifier
	    
      | SATISFIABLE ->
	  (* exhausted branch found *)
	  lemma := check_exhausted derivation
  done

	  




(*** printing around the derivation ***)

(* mark derivation start *)
let print_derivation_start derivation =
  if Config.print_derivation_online derivation.dv_config then
    print_endline ("SZS output start CNFRefutation for " ^ (Config.problem_file_name derivation.dv_config))

(* mark derivation end *)
let print_derivation_end derivation =
  if Config.print_derivation_online derivation.dv_config then
    print_endline ("SZS output end CNFRefutation for " ^ (Config.problem_file_name derivation.dv_config))

(* print used time *)
let print_derivation_time () =
  let sys_time = Sys.time ()
  and memory = ((Gc.stat ()).Gc.top_heap_words) / ((1024 * 1024 * 8) / Sys.word_size)
  in 
  print_newline ();
  Print.print_statistic_float "CPU  Time (s)" sys_time;
  Print.print_statistic       "Memory    (MB)" memory

(* print statistics *)
let print_derivation_stats derivation =
  if Config.print_statistics derivation.dv_config then begin
    print_newline ();

    Statistic.print derivation.dv_statistic;
    Context.print_max_size derivation.dv_context;
    derivation.dv_bound#print_statistic;

    Print.print_statistic "Lemmas" (List.length (Selection.get_lemmas derivation.dv_selection));
    print_derivation_time ()
  end


(* after the derivation ends finalize all output/logging *)
let print_derivation_post derivation =
  print_derivation_stats derivation;
  derivation.dv_log#finalize


(* print model *)
let print_derivation_model derivation =
  (* context to stdout *)
  if Config.print_model_context derivation.dv_config then begin
    print_endline ("\nMODEL (CONTEXT):");
    print_endline ("SZS output start Model for " ^ (Config.problem_file_name derivation.dv_config));
    Context.print_context derivation.dv_context stdout;
    print_endline ("SZS output end Model for " ^ (Config.problem_file_name derivation.dv_config));
  end;
  
  (* context to file *)
  if Config.print_model_context_file derivation.dv_config <> "" then begin
    let file_name =
      Config.print_model_context_file derivation.dv_config
    in
      try
	let out =
	  open_out file_name
	in
	  Context.print_context derivation.dv_context out;
	  close_out out
      with
	| Sys_error error_message ->
	    print_endline ("Print Context to " ^ file_name ^ " failed.");
	    print_endline error_message;
  end;
  
  (* DIG to stdout *)
  if Config.print_model_DIG derivation.dv_config then begin
    print_endline ("\nMODEL (DIG):");
    print_endline ("SZS output start Model for " ^ (Config.problem_file_name derivation.dv_config));
    Context.print_DIG derivation.dv_context stdout derivation.dv_problem;
    print_endline ("SZS output end Model for " ^ (Config.problem_file_name derivation.dv_config));
  end;
  
  (* DIG to file *)
  if Config.print_model_DIG_file derivation.dv_config <> "" then begin
    let file_name =
      Config.print_model_DIG_file derivation.dv_config
    in
      try
	let out =
	  open_out file_name
	in
	  Context.print_DIG derivation.dv_context out derivation.dv_problem;
	  close_out out
      with
	| Sys_error error_message ->
	    print_endline ("Print DIG to " ^ file_name ^ " failed.");
	    print_endline error_message;
  end;

  if Config.print_model_tptp derivation.dv_config then begin
    print_endline ("\nMODEL (TPTP):");
    print_endline ("SZS output start FiniteModel for " ^ (Config.problem_file_name derivation.dv_config));

    Context.print_tptp_model
      derivation.dv_context stdout derivation.dv_problem derivation.dv_finite_domain derivation.dv_bound;

    print_endline ("SZS output end FiniteModel for " ^ (Config.problem_file_name derivation.dv_config));
  end;

  (* TPTP to file *)
  if Config.print_model_tptp_file derivation.dv_config <> "" then begin
    let file_name =
      Config.print_model_tptp_file derivation.dv_config
    in
      try
	let out =
	  open_out file_name
	in
	  Context.print_tptp_model
	    derivation.dv_context out derivation.dv_problem derivation.dv_finite_domain derivation.dv_bound;
	  close_out out
      with
	| Sys_error error_message ->
	    print_endline ("Print tptp to " ^ file_name ^ " failed.");
	    print_endline error_message;
  end;

  if Config.print_model_finite derivation.dv_config then begin
    print_endline ("\nMODEL (Multiplication Tables):");
    print_endline ("SZS output start FiniteModel for " ^ (Config.problem_file_name derivation.dv_config));
    Context.print_multiplication_tables
      derivation.dv_context stdout derivation.dv_problem derivation.dv_bound;
    print_endline ("SZS output end FiniteModel for " ^ (Config.problem_file_name derivation.dv_config));
  end


    





(*** derivation starts / restarts ***)

(* create a derivation structure,
   based on the arguments fresh, or restarting from a previous one *)
let create_derivation (problem: problem) (finite_domain: finite_domain option)
    (lemmas: clause list)
    (config: config) (bound: bound) (statistic: statistic)
    (previous_jump: Jumping.jumping option) (is_fd_incomplete: bool)
    : derivation =

  let state =
    State.create config
  in
  let context =
    Context.create config statistic state
  in

  let clauses =
    match finite_domain with
      | None ->
	  problem#getClauses

      | Some finite_domain ->
	  let clauses =
	    (Finite_domain.get_flattened finite_domain)#getClauses
	  in

	  let print_tptp () =
	    print_endline ("START OF PROBLEM FOR SIZE " ^ string_of_int bound#current_bound);
	    Array.iteri
	      (fun i clause ->
		print_endline (
		    Term.tptp_clause_to_tptp_string
		      ("flattened_" ^ string_of_int i)
		      clause;
		)
	      )
	      (Array.of_list clauses);
	    (* just to print the axioms *)
	    let _axioms: clause list =
	      Finite_domain.get_axioms
		~print:false
		~print_tptp:true
		~use_functionality_axioms:(Config.finite_domain_functionality config)
		finite_domain bound#current_bound
	    in
	      print_endline ("END OF PROBLEM FOR SIZE " ^ string_of_int bound#current_bound);
	  in

	    begin
	      match Config.print_finite_domain_problem config with
		| Flags.PFD_Silent ->
		    ()

		| Flags.PFD_Print ->
		    print_tptp ()

		| Flags.PFD_Exit ->
		    print_tptp ();
		    exit 0;
	    end;
	    let axioms =
	      Finite_domain.get_axioms
		~print:(Config.print_finite_domain_axioms config)
		~print_tptp:false
		~use_functionality_axioms:(Config.finite_domain_functionality config)
		finite_domain bound#current_bound
	    in
	      clauses @ axioms
  in

  let jump, in_saved_branch, selection =
    match previous_jump with
      | None ->
	  (* new derivation, create things fresh *)
	  Jumping.create config statistic state bound,
	  false,
	  Selection.create config bound statistic state context finite_domain
	    clauses lemmas problem#getInitialInterpretation []

      | Some jump ->
	  (* replay skipped branch *)
	  let saved_branch =
	    Jumping.replay jump state
	  in
	  let selection =
	    Selection.create config bound statistic state context finite_domain
	      clauses lemmas problem#getInitialInterpretation saved_branch
	  in
	    jump, true, selection
  in
  let log =
    Log.create config state context
  in
    {
      dv_config = config;
      dv_bound = bound;
      dv_statistic = statistic;
      dv_state = state;
      dv_context = context;
      dv_selection = selection;
      dv_problem = problem;
      dv_log = log;

      dv_jump = jump;
      dv_in_saved_branch = in_saved_branch;

      dv_finite_domain = finite_domain;
      dv_is_fd_incomplete = is_fd_incomplete;
    }


(* restart the derivation with an increased deepending bound *)
let rec restart_increased (derivation: derivation) : unit =
  (* increase bound *)
  let old_domain_size =
    derivation.dv_bound#current_bound
  in
    derivation.dv_bound#restart ~keep_bound:false;

    (* obey maximum depth bound *)
    if Config.max_deepening_bound derivation.dv_config > 0
      &&
      Config.max_deepening_bound derivation.dv_config < derivation.dv_bound#current_bound
    then
      raise (BOUND_EXHAUSTED derivation);


    (* debug *)
    if
      Config.finite_domain derivation.dv_config
      &&
      (derivation.dv_bound#current_bound != (old_domain_size + 1))
    then begin
       print_endline (string_of_int old_domain_size);
       print_endline (string_of_int (derivation.dv_bound#current_bound));
       failwith "domain size increase by more than 1"
     end;

  let lemmas =
    if not (Config.finite_domain derivation.dv_config) then
      Selection.get_lemmas derivation.dv_selection

    else begin
      (* don't keep any propositional lemmas *)
      if Config.lemma derivation.dv_config == Flags.LM_Propositional then
	[]

      (* don't keep lemmas containing the domain guard *)
      else begin
	List.find_all
	  (fun lemma ->
	    List.for_all
	      (fun literal ->
		not (Term.is_fd_size_marker literal)
	      )
	      lemma
	  )
	  (Selection.get_lemmas derivation.dv_selection)
      end
    end
  in

  let derivation =
    create_derivation
      derivation.dv_problem
      derivation.dv_finite_domain
      lemmas
      derivation.dv_config
      derivation.dv_bound
      derivation.dv_statistic
      None
      false
  in
   
    if Config.print_derivation_online derivation.dv_config then begin
      print_endline (
	(State.active_choice_point_to_string derivation.dv_state)
	^ "Depth bound: " ^ derivation.dv_bound#complexity_to_string derivation.dv_bound#current_bound
      );
    end;

    continue derivation


(* explore the search space specified by a saved guiding path. *)
and replay_guiding_path (derivation: derivation) : unit =
  (* restart, but keep current bound *)
  derivation.dv_bound#restart ~keep_bound:true;

  let derivation =
    create_derivation
      derivation.dv_problem
      derivation.dv_finite_domain
      [] (*(Selection.get_lemmas derivation.dv_selection)*)
      derivation.dv_config
      derivation.dv_bound
      derivation.dv_statistic
      (Some derivation.dv_jump)
      derivation.dv_is_fd_incomplete (* :TODO: does this work with replay and finite domain? *)
  in

    if Config.print_derivation_online derivation.dv_config then begin
      print_endline (
	(State.active_choice_point_to_string derivation.dv_state)
	^ "Replaying Saved Branch"
      );
    end;

    continue derivation








(* start/continue the derivation, and print the result at the end of the derivation.

   handles:
   - incomplete branches, i.e. branches only exhausted within the current deepending bound.
   - saved branches, i.e. skipping and revisiting of parts of the search space
*)
and continue (derivation: derivation) : unit =
  try
    select derivation;
  with
    (* input is unsatisfiable *)
    | UNSATISFIABLE ->
	(* some parts of the search space have been skipped,
	   visit them now. *)
	if not (Jumping.finished derivation.dv_jump) then begin
	  replay_guiding_path derivation
	end

	(* in finite model mode refutations can not be found,
	   so all domain sizes for BS have to be exhausted. *)
	else if
	  Config.finite_domain derivation.dv_config
	  &&
	  not derivation.dv_is_fd_incomplete
	then begin
	  print_derivation_end derivation;
	  print_endline (get_SZS_status_line (get_SZS_unsatisfiable (Config.is_theorem derivation.dv_config)) (Config.problem_file_name derivation.dv_config));
	  print_derivation_post derivation;
	end
	  
	(* in finite model mode refutations can not be found,
	   so all domain sizes for BS have to be exhausted. *)
	else if
	  Config.finite_domain derivation.dv_config
	  &&
	  derivation.dv_problem#isBS
	  &&
	  match derivation.dv_finite_domain with
	    | None ->
		false

	    | Some finite_domain ->
		derivation.dv_bound#current_bound
		>=
		Sort_inference.max_constant_partition_size (Finite_domain.get_sorts finite_domain)
	then begin
	  (* if started with domain size 1, there are no models *)
	  if Config.deepening_bound derivation.dv_config = 1 then begin
            print_derivation_end derivation;
	    print_endline (get_SZS_status_line (get_SZS_unsatisfiable (Config.is_theorem derivation.dv_config)) (Config.problem_file_name derivation.dv_config));
	    print_derivation_post derivation;
	  end

	  else begin
	    print_endline ("No model with size >= "
			  ^ string_of_int (Config.deepening_bound derivation.dv_config)
			  ^ " exists.");
	    print_endline ("SZS status GaveUp");
	  end
	end

	(* search space complete, but contained some incomplete exhausted branches,
	   so restart with increased deepending bound *)   
	else if derivation.dv_bound#is_derivation_incomplete then begin
	  restart_increased derivation
	end


	(* can't find any refutations in finite model mode *)
	else if Config.finite_domain derivation.dv_config then begin
	  restart_increased derivation
	end

	(* input is unsatisfiable, end derivation *)
	else begin
	  print_derivation_end derivation;
	  print_endline (get_SZS_status_line (get_SZS_unsatisfiable (Config.is_theorem derivation.dv_config)) (Config.problem_file_name derivation.dv_config));
	  print_derivation_post derivation;
	end

	  
    (* exhausted branch found *)
    | SATISFIABLE ->
	(* input is satisfiable, end derivation *)
	if derivation.dv_bound#dropped_choice_point == None then begin
	  print_derivation_end derivation;
	  print_endline (get_SZS_status_line (get_SZS_satisfiable (Config.is_theorem derivation.dv_config)) (Config.problem_file_name derivation.dv_config));
	  print_derivation_model derivation;
	  print_derivation_post derivation;
	end

	(* current branch is incomplete *)
	else begin
	  (* replay the next saved path and try to find a model there. *)
	  if not (Jumping.finished derivation.dv_jump) then
	    replay_guiding_path derivation

	  (* restart with an increased deepending bound *)
	  else
	    restart_increased derivation
	end


    (* derivation aborted due to internal limit.
       Note: this case never happened in current tests,
       but there are some hard coded limits (Counter usage) *)
    | NO_SOLUTION message ->
	print_derivation_end derivation;	
	print_endline (message);	
	print_endline (get_SZS_status_line "ResourceOut" (Config.problem_file_name derivation.dv_config));
	print_derivation_post derivation
	
    (* derivation aborted by external signal. *)
    | TERM_SIGNAL signal ->
	print_derivation_stats derivation;
	print_endline (signal);
	print_endline (get_SZS_status_line "User" (Config.problem_file_name derivation.dv_config));
  
    | OUT_OF_MEMORY ->
	print_derivation_stats derivation;
	print_newline ();
	print_endline (get_SZS_status_line "MemoryOut" (Config.problem_file_name derivation.dv_config));
	
    (* just catch and print any unexpected exception.
       might want to disable this for debugging to get a failure call trace *)
    | exn ->
	print_derivation_stats derivation;
	print_endline (Printexc.to_string exn)



	  






(* start a derivation *)
let prove (config: config) problem flattened  : unit =
  let bound =
    if problem#isBS || Config.finite_domain config then
      match flattened with
	| None ->
	    Bound.create_BS (Config.deepening_bound config) config
	      
	| Some finite_domain ->
	    let max_clique_size =
	      Sort_inference.get_max_clique (Finite_domain.get_sorts finite_domain)
	    in
	    let bound =
	      if max_clique_size > Config.deepening_bound config then
		Bound.create_BS max_clique_size config
	      else
		Bound.create_BS (Config.deepening_bound config) config
	    in
	      Finite_domain.set_bound finite_domain bound;
	      bound
    else
      Bound.create ~inc:false config
  in
  let derivation: derivation =
    create_derivation problem flattened [] config bound (Statistic.create config) None false
  in
    if Config.print_configuration derivation.dv_config then begin
      Config.print derivation.dv_config;
    end;

    if Config.print_level derivation.dv_config > 0 then begin
      print_endline ("Proving " ^ (*Config.problem_file_name config ^*) " ...");
      print_newline ();
    end;

    print_derivation_start derivation;

    if Config.print_derivation_online derivation.dv_config then begin
      print_endline (
	(State.active_choice_point_to_string derivation.dv_state)
	^ "Depth bound: " ^ derivation.dv_bound#complexity_to_string derivation.dv_bound#current_bound
      );
    end;

    if Config.max_deepening_bound derivation.dv_config > 0
      &&
      Config.max_deepening_bound derivation.dv_config < derivation.dv_bound#current_bound
    then
      raise (BOUND_EXHAUSTED derivation);

    continue derivation


















(*** input processing **)

(* decide input format by the file name suffix, or by assuming a default. *)
let check_suffix (file_name: string) (default: Flags.opt_input_format) : Flags.opt_input_format =
  if Filename.check_suffix file_name ".darwin" then
    Flags.FI_Darwin
      
  else if Filename.check_suffix file_name ".tme" then
    Flags.FI_TME
	
  else if Filename.check_suffix file_name ".tptp" then
    Flags.FI_TPTP

  else begin
    begin
      match default with
	| Flags.FI_TME ->
	    print_endline ("Defaulting to tme format.")
	      
	| Flags.FI_TPTP ->
	    print_endline ("Defaulting to tptp format.")

	| Flags.FI_Darwin ->
	    print_endline ("Defaulting to darwin format.")
    end;
    default;
  end




(* check that eprover is available and return its name,
   either relative to the current path or within the environment program path.

   :TODO: might check E version here

   :TODO: should add the time used by the clausifier to the total run time.
*)
let find_eprover (flags: flags) : string =
  let rec find_eprover' provers' =
    match provers' with
      | [] ->
	  raise Not_found
	    
      | prover :: tail ->
	  let in_channel, _p_out, _p_error =
	    Unix.open_process_full (prover ^ " --version") (Unix.environment ())
	  in
	    begin
	      try
		let _version =
		  input_line in_channel
		in
		  ignore (Unix.close_process_full (in_channel, _p_out, _p_error) : Unix.process_status);
		  (* should probably check version here... *)
		  prover
	      with
		| End_of_file ->
		    (* prover not found *)
		    find_eprover' tail
		    
	    end;
  in
  let search_paths =
      [
	(* first try the execution path of darwin *)
	Filename.concat (Filename.dirname Sys.argv.(0)) "eprover";
	(* then general program path *)
	"eprover"
      ]
  in

    (* eprover explicitely specified *)
    if (Flags.eprover flags)#value = "" then begin
      try
	find_eprover' search_paths
      with
	| Not_found ->
	  print_endline ("");
	  print_endline ("Couldn't find eprover, need version 0.91 or higher to clausify input.");
	  print_endline ("Specify path to eprover with flag '--eprover'.");
	  print_endline ("SZS status GaveUp(FOF)");
	  exit 0;
    end
    else begin
      try
	find_eprover' [ (Flags.eprover flags)#value]
      with
	| Not_found ->
	    print_endline ("");
	    print_endline ("WARNING: Couldn't find eprover at " ^ (Flags.eprover flags)#value);
	    print_endline ("Searching for eprover in execution and standard path ...");
	    try
	      let path = find_eprover' search_paths in
		print_endline ("Using eprover at " ^ path);
		print_endline ("");
		path
	    with
	      | Not_found ->
		  print_endline ("");
		  print_endline ("Couldn't find eprover, need version 0.91 or higher to clausify input.");
		  print_endline ("SZS status GaveUp(FOF)");
		  exit 0;
    end



(* clausify a tptp problem with the eprover *)
let clausify in_channel =
  let success =
    ref false
  in

  let rec clausify in_channel clausified =
    try
      let line =
	input_line in_channel
      in
	(* ignore blank lines *)
	if String.length line == 0 then begin
	  clausify in_channel clausified
        end

	else if line = "# CNFization successful!" then begin
	  success := true;
	  clausify in_channel clausified
	end

	else if line = "# SZS status ResourceOut" then begin
	  raise Const.CLAUSIFIER_RESOURCE_OUT
	end
	    
	(* ignore eprover comments *)
	else if String.get line 0 = '#' then begin
 	  clausify in_channel clausified
	end
	    
	else
	  line :: clausify in_channel clausified
    with
      | End_of_file ->
	  List.rev clausified
  in
  let clausified =
    clausify in_channel []
  in
    close_in in_channel;
    
    if not !success then begin
      print_endline ("");
      print_endline ("Failed to clausify problem.");
      print_endline ("SZS status GaveUp(Clausifier)");
      exit 0;
    end;

    clausified


let clausify_string (flags: flags) (problem : string) (timeout: int) : string list =
  let eprover =
    find_eprover flags
  in

  let eprover_flags =
    if (Flags.preprocess_clausifier flags)#value then
      " --tstp-format --silent --cnf "
    else
      " --tstp-format --silent --cnf --no-preprocessing "
  in
  let command =
    if timeout > 0 then
      (eprover ^ eprover_flags ^ "--cpu-limit=" ^ string_of_int timeout)
    else
      (eprover ^ eprover_flags)
  in
  (* create clausifier with in and out channel *)
  let in_channel, out_channel =
    Unix.open_process command
  in

    (* pipe the problem to the clausifier *)
    output_string out_channel problem;
    close_out out_channel;

    (* get the clausified problem *)
    clausify in_channel


let clausify_file (flags: flags) (file_name : string) (timeout: int) : string list =
  let eprover =
    find_eprover flags
  in
    if (Flags.print_level flags)#value > 0 then
      print_endline ("Calling " ^ eprover ^ " for clausification ...");

  let eprover_flags =
    if (Flags.preprocess_pure flags)#value then
      " --tstp-format --silent --cnf "
    else
      " --tstp-format --silent --cnf --no-preprocessing "
  in
  let command =
    if timeout > 0 then
      (eprover ^ eprover_flags ^ "--cpu-limit=" ^ string_of_int timeout ^ " " ^ file_name)
    else
      (eprover ^ eprover_flags ^ file_name)
  in
  (* create clausifier with out channel *)
  let in_channel =
    Unix.open_process_in command
  in
    (* get the clausified problem *)
    clausify in_channel


(* read and parse input from string.
   returns (fof, theorem) iff input is in FOF format and the task is to prove a theorem
*)
let read_clauses_from_string (flags: flags) (problem: string) (format: Flags.opt_input_format) (timeout: int) :
    clause list * bool * bool =
  match format with
    | Flags.FI_Darwin ->
	Read_darwin.to_clauses_from_string problem, false, false
	  
    | Flags.FI_TME ->
	Read_tme.to_clauses_from_string problem, false, false

    | Flags.FI_TPTP ->
	begin
	  try
	    Read_tptp.to_clauses_from_string problem, false, false
	  with
	    | Const.FOF theorem ->
		let clausified =
		  clausify_string flags problem timeout
		in
		  List.flatten (
		    List.map
		      (fun line -> Read_tptp.to_clauses_from_string line)
		      clausified
		  ),
		  true,
		  theorem
	end


(* read and parse input from file.
   returns (fof, theorem) iff input is in FOF format and the task is to prove a theorem
*)
let read_clauses_from_file (flags: flags) (file_name: string) (format: Flags.opt_input_format) (timeout: int)
    : clause list * bool * bool =
  try
    match format with
      | Flags.FI_Darwin ->
	  Read_darwin.to_clauses_from_file file_name, false, false
	
      | Flags.FI_TME ->
	  Read_tme.to_clauses_from_file file_name, false, false

      | Flags.FI_TPTP ->
	  begin
	    try
	      Read_tptp.to_clauses_from_file file_name, false, false
	    with
	      | Const.FOF theorem ->
		  let clausified =
		    clausify_file flags file_name timeout
		  in
		    List.flatten (
		      List.map
			(fun line -> Read_tptp.to_clauses_from_string line)
			clausified
		    ),
		    true,
		    theorem
	  end
  with
    | Sys_error error_message ->
	print_endline error_message;
	exit 0



(* read the input problem as specified in the flags.
   returns (fof, theorem) iff input is in FOF format and the task is to prove a theorem
*)
let read_input (flags: flags) (timeout: int) : clause list * bool * bool =
  let input_file_name =
    Flags.problem_file_name flags
  in
    
  (* file format by suffix (or default *)
  let format =
    check_suffix input_file_name (Flags.input_format flags)#value
  in
  let zip_file_name =
    Flags.zipped_file_name flags
  in
    (* read zipped input *)
    if zip_file_name <> "" then begin
      print_endline ("Reading compressed file " ^ zip_file_name ^ " ...");
      let zip_file =
	try
	  Zip_wrapper.open_in zip_file_name
	with
	  | Sys_error error_message ->
	      print_endline error_message;
	      exit 0;
      in
	if (Flags.print_level flags)#value > 0 then
	  print_endline ("Parsing " ^ input_file_name ^ " from compressed file ...");      
	let entry =
	  try
	    Zip_wrapper.find_entry zip_file input_file_name
	  with
	    | Not_found ->
		failwith ("File " ^ input_file_name ^ " not in compressed file " ^ zip_file_name)
	in
	let problem =
	  Zip_wrapper.read_entry zip_file entry
	in
	  read_clauses_from_string flags problem format timeout
    end

    else begin
      if (Flags.print_level flags)#value > 0 then
	print_endline ("Parsing " ^ input_file_name ^ " ...");      
      read_clauses_from_file flags input_file_name format timeout
    end



(* do all preprocessing steps.
   might raise EMPTY_CLAUSE of any preprocessing module. *)
let preprocess (flags: flags) (clauses: clause list) : Problem.problem * finite_domain option =
  let input_contains_equality =
    Term.contains_equality clauses
  in

  (* needed for soundness to remove truth literals from clauses,
     otherwise true can be interpreted as false and vice versa. *)
  let clauses =
    Tools.list_map_find
      (fun clause -> not (List.exists (fun l -> Term.literal_equal Term.true_literal l) clause))
      (fun clause -> List.find_all (fun l -> not (Term.literal_equal Term.false_literal l)) clause)
      clauses
  in
  let clauses, simplifiedTruth =
    List.partition
      (fun clause ->
	 not (List.exists (Term.literal_equal Term.true_literal) clause)
      )
      clauses
  in
  let clauses =
    List.map
      (List.find_all
	 (fun l -> not (Term.literal_equal Term.false_literal l))
      )
      clauses
  in
  let clauses, simplifiedTautology =
    List.partition
      (fun clause ->
	not (Term.is_tautology clause)
      )
      clauses
  in

  let (clauses, simplifiedUnit) =
    if (Flags.preprocess_unit flags)#value then
      Preprocessing_unit.simplify ~print:((Flags.print_preprocess_unit flags)#value) clauses
    else
      (clauses, [])
  in

  let clauses, simplifiedPure, initial_interpretation =
    if (Flags.preprocess_pure flags)#value then
      Preprocessing_pure.simplify
	~print:(Flags.print_preprocess_pure flags)#value
	~equality:input_contains_equality
	~finite_domain:(Flags.finite_domain flags)#value
	clauses
    else
      clauses, [], []
  in

  let clauses =
    match (Flags.preprocess_split flags)#value with
      | Flags.PPS_None ->
	  clauses
	    
      | Flags.PPS_Ground ->
	  Preprocessing_split_ground.split
	    ~print:(Flags.print_preprocess_split flags)#value
	    clauses
	    
      | Flags.PPS_NonGround ->
	  let ground =
	    Preprocessing_split_ground.split
	      ~print:(Flags.print_preprocess_split flags)#value
	      clauses
	  in
	    Preprocessing_split_nonground.split
	      ~print:(Flags.print_preprocess_split flags)#value
	      ground
  in
      
  let clauses, simplifiedEquality =
    if input_contains_equality && (Flags.preprocess_equality flags)#value then
      Preprocessing_equality.simplify ~print:(Flags.print_preprocess_equality flags)#value clauses
    else
      clauses, []
  in

  (* normalize all clauses - makes weak subsumption tests more efficient *)
  let clauses =
    List.map
      (fun clause -> Subst.normalize_clause (Term.sort_clause clause))
      clauses
  in

  (* sort clauses by length *)

  let clauses =
	List.sort
	  (fun x y ->
	    let length =
	      Tools.compare_int (List.length x) (List.length y)
	    in
	      if length != 0 then
		length
	      else
		Tools.compare_int (Term_attributes.weight_of_clause x) (Term_attributes.weight_of_clause y)
	  )
	  clauses
  in
(*  let clauses =
    List.sort
      (fun x y -> Tools.compare_int (List.length x) (List.length y))
      clauses
  in*)

  let clauses =
    if (Flags.preprocess_resolution flags)#value then begin
      let resolvents =
	Preprocessing_resolution.compute_resolvents
	  ~print:(Flags.print_preprocess_resolution flags)#value
	  clauses
      in
      let resolvents =
	match (Flags.preprocess_split flags)#value with
	  | Flags.PPS_None ->
	      resolvents

	  | Flags.PPS_Ground ->
	      Preprocessing_split_ground.split ~print:false resolvents
		
	  | Flags.PPS_NonGround ->
	      let ground =
		Preprocessing_split_ground.split ~print:false resolvents
	      in
		Preprocessing_split_nonground.split ~print:false ground
      in
      let resolvents, _simplifiedEquality =
	if input_contains_equality then
	  Preprocessing_equality.simplify ~print:false resolvents
	else
	  resolvents, []
      in
(*	resolvents @ clauses*)
  let clauses =
	List.sort
	  (fun x y ->
	    let length =
	      Tools.compare_int (List.length x) (List.length y)
	    in
	      if length != 0 then
		length
	      else
		Tools.compare_int (Term_attributes.weight_of_clause x) (Term_attributes.weight_of_clause y)
	  )
	  (clauses @ resolvents)
  in
    clauses
    end
    else
      clauses
  in

  let simplified = [
    simplifiedTruth; simplifiedTautology; simplifiedUnit; simplifiedPure; simplifiedEquality
  ] in
  let problem =
    Problem.create clauses simplified initial_interpretation
  in
  let flattened =
    (* finite model mode *)  
    if (Flags.finite_domain flags)#value then begin
      let finite_domain =
	Finite_domain.create
	  ~print_transformation:(Flags.print_finite_domain_transformation flags)#value
	  ~print_sorts:(Flags.print_finite_domain_sorts flags)#value
	  problem None (Flags.unique_name_assumption flags)#value
      in
	Some finite_domain
    end
    (* default mode *)
    else begin
      (* add equality axioms *)
      if problem#containsEquality && (Flags.equality flags)#value = Flags.EQ_Axioms then begin
	  let axioms =
	    Equality.get_axioms ~print_axioms:(Flags.print_equality_axioms flags)#value problem
	  in
	    problem#addClauses axioms
      end;
      None
    end
  in
    problem, flattened



(* setup a derivation based on command line arguments. *)
let () =
  init_signal_handler ();

  let start_time =
    Sys.time ()
  in

  (* read command line flags *)
  let flags =
    Flags.eval_command_line ()
  in
    (* set time out if indicated by flags *)
    set_time_outs flags;
    (* set memory out *)
    ignore (Gc.create_alarm
	     (handle_memory_limit
	       (Flags.memory_limit flags)#value)
	     : Gc.alarm);

  try
    let clauses, fof, theorem =
      read_input flags (int_of_float (Flags.time_out_CPU flags)#value)
    in
      if (Flags.print_level flags)#value > 0 then
	print_newline ();

    (* catch empty clause.
       e.g. false :- true. in TPTP problem SWC128-1 *)
    if Term.contains_empty_clause clauses then begin
      print_endline ("Input contains empty clause.\n");
      print_endline (get_SZS_status_line (get_SZS_unsatisfiable theorem) (Flags.problem_file_name flags))
    end
	
    else begin
    try
      let problem, flattened =
	preprocess flags clauses
      in
	if (Flags.print_level flags)#value > 0 then
	  print_newline ();

      let config =
	Config.create
	  ~flags:flags
	  ~problem:problem
	  ~is_horn:(not (Flags.finite_domain flags)#value)
	  ~start_time:start_time
          ~fof:fof
	  ~theorem:theorem
      in
	if (Flags.print_level flags)#value > 0 then
	  print_newline ();

	prove config problem flattened;
    with
      | Preprocessing_unit.EMPTY_CLAUSE clause ->
	  print_endline ("Clause simplifiable to empty clause:");
	  print_endline (Term.clause_to_string clause);
	  print_newline ();
	  print_endline (get_SZS_status_line (get_SZS_unsatisfiable theorem) (Flags.problem_file_name flags))
	  
      | Preprocessing_equality.EMPTY_CLAUSE clause ->
	  print_endline ("Clause simplifiable to empty clause:");
	  print_endline (Term.clause_to_string clause);
	  print_newline ();
	  print_endline (get_SZS_status_line (get_SZS_unsatisfiable theorem) (Flags.problem_file_name flags))
	  
      | Preprocessing_resolution.EMPTY_CLAUSE (first, second) ->
	  print_endline ("Clauses resolve to empty clause:");
	  print_endline (Term.clause_to_string first);
	  print_endline (Term.clause_to_string second);
	  print_newline ();
	  print_endline (get_SZS_status_line (get_SZS_unsatisfiable theorem) (Flags.problem_file_name flags))
      end
  with
    | Const.PARSE_ERROR ->
	print_newline ();
	print_endline (get_SZS_status_line "SyntaxError"  (Flags.problem_file_name flags))

    | Const.CLAUSIFIER_RESOURCE_OUT ->
	print_endline ("Clausifier ran out of resources.");
	print_newline ();
	print_endline (get_SZS_status_line "ResourceOut"  (Flags.problem_file_name flags))

    | TERM_SIGNAL signal ->
	print_derivation_time ();
	print_endline (signal)

    | OUT_OF_MEMORY ->
	print_newline ();
	print_endline (get_SZS_status_line "MemoryOut"  (Flags.problem_file_name flags))

    | BOUND_EXHAUSTED derivation ->
	let max_bound = Config.max_deepening_bound derivation.dv_config in
	if Config.finite_domain derivation.dv_config then
	  print_endline ("Maximum domain size reached: " ^ string_of_int max_bound)
	else
	  print_endline ("Maximum depth limit reached: " ^ string_of_int max_bound);
	print_newline ();
	print_endline (get_SZS_status_line "ResourceOut"  (Config.problem_file_name derivation.dv_config))
