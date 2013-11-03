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


(*** types ***)

type config = Config.config
type bound = Bound.bound
type statistic = Statistic.statistic
type literal = Term.literal
type clause = Term.clause
type subst = Subst.subst
type choice_point = State.choice_point
type state = State.state
type context_unifier_space = Context_unifier.context_unifier_space
type space_registry = Context_unifier_space.space_registry
type raw_context_unifier = Context_unifier.raw_context_unifier
type selected = Selection_types.selected
type problem_literals = Problem_literals.problem_literals
type context = Context.context
type context_element = Context.element
type guiding_step = Jumping.guiding_step
type guiding_path = Jumping.guiding_path
type lemmas_propositional = Lemma_propositional.lemmas_propositional
type sorts = Sort_inference.sorts
type finite_domain = Finite_domain.finite_domain

type selection = {
  (* environment *)
  sel_config: config;
  sel_bound: bound;
  sel_statistic: statistic;
  sel_state: state;
  sel_context: context;
  sel_space_registry: space_registry;
  sel_finite_domain: finite_domain option;

  (* storage for the literals of the input clause set.
     triggers search for context unifiers. *)
  sel_problem_literals: problem_literals;

  (* the context_unifier_spaces build on the input clause set *)
  sel_clause_spaces: context_unifier_space array;

  (* the context_unifier_spaces build on the lemmas.
     bool: true if a real lemma which can be kept over restarting.
  *)
  mutable sel_lemma_spaces: (context_unifier_space * bool) list;

  (* the best closing context unifiers found during the last candidate computation. *)
  sel_best_closing: raw_context_unifier option ref;

  (* assert candidates from unit clauses of the problem clause set *)
  mutable sel_unit_asserts: (literal * raw_context_unifier) list;

  (* assert candidates *)
  mutable sel_assert: Selection_assert.candidates;

  (* split candidates *)
  mutable sel_split: Selection_split.candidates;

  (* deferred computation of split candidates:
     split candidate are not computed at all as long as assert candidates
     are available. then, after split candidates are needed once,
     they are computed along with the assert candidates.

     true, if no split candidates were computed yet,
     i.e. only close and assert context unifiers are currently computed.
  *)
  mutable sel_no_splits_yet: bool;

  (* call back functions to be called by the spaces on the clauses
     when new candidates are computed *)
  sel_process_close: raw_context_unifier -> unit;
  sel_process_assert: raw_context_unifier -> bool;
  sel_process_split: raw_context_unifier -> unit;
  sel_is_element_incomplete_assert: Context.element -> bool;
  sel_is_element_incomplete_split: Context.element -> bool;


  (* counter for the number of backtracks happened.
     used to decrease the clause utilities once in a while. *)
  mutable sel_decay_counter: int ref;


  (* the stored guiding path which is currently replayed.
     on every replay its first element is removed,
     and when it is empty, a standard derivation is done. *)
  mutable sel_guiding_path: guiding_path;

  (* propositional lemmas *)
  mutable sel_lemmas_propositional: lemmas_propositional option;
}






exception SATISFIABLE




(*** creation ***)

(* old hint on non-chronological backtracking:
   
   one context literal may lead to several closing context unifiers.
   then, invalidating the current choice point
   invalidates all these context unifiers, which is ok.

   but, if one tries to be more clever and invalidates another choice point instead,
   one must either assure that all the closing context unifiers
   that do not depend on this invalidated choice point are not lost,
   but still used to close.
*)

(*
  closing introduces non-determinism in the computation.
  if several closing context unifiers are computed on one new context literal,
  the order of their computation influences which on is chosen.
  this is especially the case,
  if the closing context unifier is chosen by means of
  the assert lookahead (Selection_lookahed),
  as then the first one is taken and the others are not even computed anymore.
*)
let process_close_candidate (state: state) (best_closing: raw_context_unifier option ref)
    (candidate: raw_context_unifier) : unit =
(*  raise (Context_unifier.CLOSE candidate)*)

  let explanation =
    State.get_explanation
      state
      (Context_unifier.get_context_literals candidate.Context_unifier.rcu_context_partners)
  in
    (* immediately close if the root choice point is invalidated *)
    begin
      match explanation with
	| [] ->
	    raise (Context_unifier.CLOSE candidate)

	| head :: [] when
	    State.choice_point_equal (State.root_choice_point state) head ->
	    raise (Context_unifier.CLOSE candidate)
	      
	| _ ->
	    ()
    end;
    
    begin
      match !best_closing with
	| None ->
	    (* first found closing unifier, store it *)
	    best_closing := Some candidate
	      
	| Some old ->
	    (* compare with the other stored unifier and keep the best *)
	    let old_explanation =
	      State.get_explanation
		state
		(Context_unifier.get_context_literals old.Context_unifier.rcu_context_partners)
	    in

	    (* prefer the unifier with the older choice points,
	       or with less choice points in its explanation. *)
	    let rec compare_explanations x y =
	      match x, y with
		| [], [] ->
		    (* make the comparison total to have a stable derivation *)
		    Context_unifier.compare_context_unifiers ~different:true candidate old
		      
		| [], _ ->
		    -1
		      
		| _, [] ->
		    1
		      
		| x0 :: t0, x1 :: t1 ->
		    if State.choice_point_equal x0 x1 then
		      compare_explanations t0 t1
		    else
		      State.compare_age x0 x1
	    in
	      if compare_explanations explanation old_explanation < 0 then
		best_closing := Some candidate
    end


(* create a context_unifier_space for every clause,
   gather all candidates computed on creation, i.e. with -v *)
let create_spaces
    (space_registry: space_registry) (statistic: statistic) (problem_literals: problem_literals)
    process_close_candidate process_assert_candidate process_split_candidate
    is_element_incomplete_assert is_element_incomplete_split
    (clauses: clause list) (is_lemma: bool) sorts
    : context_unifier_space list * (literal * raw_context_unifier) list =
  List.fold_left
    (fun (context_unifier_spaces, unit_asserts) clause ->
       let new_context_unifier_space : context_unifier_space =
	 Context_unifier_space.create_space space_registry problem_literals
	   clause
	   process_close_candidate process_assert_candidate process_split_candidate
	   is_element_incomplete_assert is_element_incomplete_split
	   is_lemma
	   sorts
       in

       let new_unit_asserts =
	 (* don't check original clause, but clause without constraints,
	    otherwise we miss this assert. *)
	 match new_context_unifier_space.Context_unifier.cus_clause with
	   | unit_literal :: [] ->
	       let solutions =
		 match new_context_unifier_space.Context_unifier.cus_constraints with
		   | None -> (Subst.empty, None) :: []
		   | Some constraints ->
		       (* unit assert, so empty substitution *)
		       Finite_domain_constraints.get_solutions
			 (Finite_domain_constraints.setup constraints Subst.empty)
	       in
		 List.fold_left
		   (fun acc (subst, constraints) ->
		      let unit_literal' =
			Subst.apply_to_literal subst unit_literal Subst.input_literal_offset
		      in
		      let raw_context_unifier =
			Context_unifier.create_context_unifier
			  new_context_unifier_space [| Context_unifier.assert_partner |] false
			  constraints
		      in
			Statistic.inc_computed_assert_candidates statistic;
			(unit_literal', raw_context_unifier) :: acc
		   )
		   unit_asserts
		   solutions
		   
	   | _ ->
	       unit_asserts
       in
	 (new_context_unifier_space :: context_unifier_spaces),
         new_unit_asserts
    )
    ([], [])
    clauses



let create (config: config) (bound: bound) (statistic: statistic) (state: state) (context: context)
    (finite_domain: finite_domain option) 
    (clauses: clause list) (lemmas: clause list) (initial_interpretation: literal list)
    (guiding_path: guiding_path) : selection =

  let problem_literals =
    Problem_literals.create config bound statistic state context
  in
  let space_registry =
    Context_unifier_space.create config bound
  in
  let selection_assert =
    Selection_assert.create config bound statistic state context problem_literals
  and selection_split =
    Selection_split.create config bound statistic state context problem_literals space_registry
  in

  let best_closing =
    ref None
  in
  let process_close_candidate (candidate: raw_context_unifier) : unit =
    process_close_candidate state best_closing candidate
  and process_assert_candidate (candidate: raw_context_unifier) : bool =
    Selection_assert.add selection_assert candidate
  and process_split_candidate (candidate: raw_context_unifier) : unit =
    Selection_split.add selection_split candidate
  and is_element_incomplete_assert (element: Context.element) : bool =
    Selection_assert.is_element_incomplete selection_assert element
  and is_element_incomplete_split (element: Context.element) : bool =
    Selection_split.is_element_incomplete selection_split element
  in

  let clause_spaces, unit_asserts =
    create_spaces space_registry statistic problem_literals
      process_close_candidate process_assert_candidate process_split_candidate
      is_element_incomplete_assert is_element_incomplete_split
      clauses false finite_domain
  in

  let lemma_spaces, lemma_asserts, lemmas_propositional =
    match Config.lemma config with
      | Flags.LM_Propositional ->
	  let lemmas_propositional =
	    Lemma_propositional.create config state context selection_assert process_close_candidate process_assert_candidate space_registry
	  in
	    List.iter
	  (fun lemma ->
	     Lemma_propositional.add_lemma lemmas_propositional lemma true
	  )
	  lemmas;
	  [], [], Some lemmas_propositional

      | Flags.LM_None
      | Flags.LM_Grounded
      | Flags.LM_Lifted ->

      let lemma_spaces, lemma_asserts =
	create_spaces space_registry statistic problem_literals
	  process_close_candidate process_assert_candidate process_split_candidate
	  is_element_incomplete_assert is_element_incomplete_split
	  lemmas true finite_domain
      in
      let lemma_spaces =
	List.map (fun lemma -> (lemma, true)) lemma_spaces
      in
	lemma_spaces, lemma_asserts, None
  in
    
  let initial_interpretation' =
    List.map
      (fun literal ->
	let new_context_unifier_space : context_unifier_space =
	  Context_unifier_space.create_space ~register:false
	    space_registry
	    problem_literals
	    [literal]
	    process_close_candidate
	    process_assert_candidate
	    process_split_candidate
	    is_element_incomplete_assert
	    is_element_incomplete_split
	    false
	    finite_domain
	in      
	  if Const.debug && List.length new_context_unifier_space.Context_unifier.cus_clause == 0 then
	    failwith ("Selection.create: initial interpretation contains constraint: "
		      ^ Term.literal_to_string literal);

	let raw_context_unifier =
	  Context_unifier.create_context_unifier
	    new_context_unifier_space [| Context_unifier.assert_partner |] false
	    (* initial interpretation should be based on the problem input alone, so no constraints here *)
	    None
	in
	  (literal, raw_context_unifier)
      )
      initial_interpretation
  in

  (* sort the assert literals of unit clauses by term_weight *)
  let sorted_unit_asserts =
    List.sort
      (fun (x, _) (y, _) ->
	 compare
	   (Term_attributes.weight_of_literal x)
	   (Term_attributes.weight_of_literal y)
	  )
      (initial_interpretation' @ lemma_asserts @ unit_asserts)
  in

    
  let selection = {
    sel_config = config;
    sel_bound = bound;
    sel_statistic = statistic;
    sel_state = state;
    sel_context = context;
    sel_finite_domain = finite_domain;
    sel_problem_literals = problem_literals;
    sel_space_registry = space_registry;
    sel_clause_spaces = Array.of_list clause_spaces;
    sel_lemma_spaces = lemma_spaces;
    sel_best_closing = best_closing;
    sel_unit_asserts = sorted_unit_asserts;
    sel_assert = selection_assert;
    sel_split = selection_split;
    sel_no_splits_yet = true;
    sel_process_close = process_close_candidate;
    sel_process_assert = process_assert_candidate;
    sel_process_split = process_split_candidate;
    sel_is_element_incomplete_assert = is_element_incomplete_assert;
    sel_is_element_incomplete_split = is_element_incomplete_split;
    sel_decay_counter = ref 0;
    sel_guiding_path = guiding_path;
    sel_lemmas_propositional = lemmas_propositional;
  }
  in
    selection





(*** add ***)

(* raise Close if a closing context unifier has been found *)
let raise_close (selection: selection) : unit =
  match !(selection.sel_best_closing) with
    | Some closing ->	
	selection.sel_best_closing := None;
	raise (Context_unifier.CLOSE closing)
	    
    | None ->
	()


(* add the new context literal to all unsubsumed clauses
   and get the new candidates *)
let add (selection: selection) (context_element: Context.element) : unit =
  (* propositional lemmas does its own propagation. *)
  begin
    match Config.lemma selection.sel_config with
      | Flags.LM_Propositional ->
	  begin
	    match selection.sel_lemmas_propositional with
	      | Some lemmas_propositional ->
		  Lemma_propositional.extend_context lemmas_propositional context_element;
	      | None ->
		  failwith "Selection: lemmas propositional not created"
	  end

      | Flags.LM_None
      | Flags.LM_Grounded
      | Flags.LM_Lifted ->
	  ()
  end;

  (* compute only assert candidates *)
  if selection.sel_no_splits_yet then
    Problem_literals.add selection.sel_problem_literals context_element Context_unifier.CloseAssert

  (* compute all candidates *)
  else
    Problem_literals.add selection.sel_problem_literals context_element Context_unifier.All;

  (* raise Close, if a closing context unifier has been found *)
  raise_close selection







(*** lemmas ***)



(* ignore a lemma if it is a variant of an already existing clause or lemma.
   this is only an incomplete test as we don't want to do a full subsumption test. *)
let filter_lemma (selection: selection) (lemma: clause) : bool =

  (* quick variant check, succeeds only if clauses have the same literal order *)
  let rec clauses_equal x y =
    match x, y with
      | [], [] ->
	  true
	    
      | hx :: tx, hy :: ty when
	  Term.literal_equal hx hy
	  ||
	  Term.are_literals_variants hx hy
	  ->
	  clauses_equal tx ty
	    
      | _ ->
	  false
  in

  let lemma_length =
    List.length lemma
  in

  let are_spaces_equal old =
    let x =
    lemma_length = Array.length old.Context_unifier.cus_input_partners
    &&
    (
      clauses_equal
	old.Context_unifier.cus_clause
	lemma	 
    )
    in
      (*if x then
	print_endline ("Variant of clause: " ^ Term.clause_to_string old.Context_unifier.cus_clause);*)
      (* make old clause a lemma, so it will propagate on parametric asserts *)
      Context_unifier.increase_lemma_learned old;
      x
  in
    (* variant of existing clause? *)
    Tools.array_exists
      are_spaces_equal 
      selection.sel_clause_spaces
    ||

    (* variant of existing lemma *)
    List.exists
      (fun (x, _) ->
	 are_spaces_equal x
      )
      selection.sel_lemma_spaces


(* get the worst utility value of all lemmas *)
let get_min_lemma_utility (selection: selection) : int =
  let min =
    List.fold_left
      (fun acc (space, _) ->
	 match acc with
	   | Some old ->
	       if old <= !(space.Context_unifier.cus_utility) then
		 acc
	       else
		 Some !(space.Context_unifier.cus_utility)
	       
	   | _ ->
	       Some !(space.Context_unifier.cus_utility)
      )
      None
      selection.sel_lemma_spaces
  in
    match min with
      | None ->
	  0
	    
      | Some value ->
	  value


(* remove old (bad) lemmas if there are too many *)
let prune_lemmas (selection: selection) : unit =
  let max_lemmas = Config.lemma_max selection.sel_config
  and min_lemmas = Config.lemma_min selection.sel_config
  in
  (* are there too many lemmas? *)
  if max_lemmas > 0 && List.length selection.sel_lemma_spaces >= max_lemmas then begin
(*    print_newline ();
    print_endline ("prune lemmas: " ^ string_of_int (List.length selection.sel_lemma_spaces));*)


    (* remove lemmas until there are too few left *)
    while List.length selection.sel_lemma_spaces > min_lemmas do
      let min =
	get_min_lemma_utility selection
      in
	selection.sel_lemma_spaces <-
	  List.find_all
	  (fun (space, _) ->
	     begin
	       let utility = !(space.Context_unifier.cus_utility)
	       in
	       let keep = utility > min
	       in
		 if not keep then begin
		   Problem_literals.unregister_space selection.sel_problem_literals space
		 end;
		 keep
	     end;
	  )
	  selection.sel_lemma_spaces
    done;

  end



(* remove old (bad) lemmas if there are too many *)
let prune_lemmas (selection: selection) : unit =
  let max_lemmas = Config.lemma_max selection.sel_config
  and min_lemmas = Config.lemma_min selection.sel_config
  in
  (* are there too many lemmas? *)
  if max_lemmas > 0 && List.length selection.sel_lemma_spaces >= max_lemmas then begin

    let rec find_min min =
      let pruned =
	List.fold_left
	  (fun pruned (space, _) ->
	     if !(space.Context_unifier.cus_utility) > min then
	       pruned
	     else
	       pruned + 1
	  )
	  0
	  selection.sel_lemma_spaces
      in
	if pruned >= (max_lemmas - min_lemmas) then
	  pruned
	else
	  find_min (min + 1)
    in
    let min =
      find_min (get_min_lemma_utility selection)
    in

    let rec prune pruned acc spaces =
      match spaces with
	| [] ->
	    acc

	| ((space, _) as head) :: tail ->
	     if
	       (pruned >= (max_lemmas - min_lemmas))
	       ||
	       (!(space.Context_unifier.cus_utility) > min)
	     then begin
	       prune pruned (head :: acc) tail
	     end

	     else begin
	       Problem_literals.unregister_space selection.sel_problem_literals space;
	       prune (pruned + 1) acc tail
	     end
    in
    let keep =
      prune 0 [] selection.sel_lemma_spaces
    in
(*
    let keep =
      let pruned =
	ref 0
      in
	List.find_all
	  (fun (space, _) ->
	     if
	       (!pruned >= (Const.max_stored_lemmas - Const.min_stored_lemmas))
	       ||
	       (!(space.Context_unifier.cus_utility) > min)
	     then begin
	       true
	     end

	     else begin
	       pruned := pruned + 1;
	       false
	     end
	       
	  )
	  (* on tie remove oldest first *)
	  (List. rev selection.sel_lemma_spaces)
    in*)
      selection.sel_lemma_spaces <- keep;

    Array.iter
      (fun space ->
	 space.Context_unifier.cus_utility :=
	   !(space.Context_unifier.cus_utility) / Const.decay_clause_utility_ratio
      )
      selection.sel_clause_spaces;

    List.iter
      (fun (space, _) ->
	 space.Context_unifier.cus_utility :=
	   !(space.Context_unifier.cus_utility) / Const.decay_clause_utility_ratio
      )
      selection.sel_lemma_spaces
  end





let add_lemma (selection: selection) (lemma: clause) : unit =
  if Const.debug then begin
    match selection.sel_finite_domain with
      | Some finite_domain ->
	  List.iter
	    (fun var ->
	       ignore (Sort_inference.get_var_sort
			 (Finite_domain.get_sorts finite_domain) lemma var
			 : Sort_inference.sort option);
	    )
	    (Term.vars_of_clause lemma)
      | None -> ()
  end;

  (* some simplification in finite domain mode *)
  try
  let lemma =
    if Config.finite_domain selection.sel_config then
      Lemma.remove_falsified_constraints lemma
    else
      lemma
  in

  let lemma =
    if
      Const.fd_isomorphism_abstraction 
      &&
      Config.finite_domain selection.sel_config
    then
      match selection.sel_finite_domain with
	| Some finite_domain -> Lemma.abstract_permutable lemma (Finite_domain.get_sorts finite_domain)
	| None -> failwith "Selection.add_lemma: no sorts given in finite domain mode."
    else
      lemma
  in

  (* ignore redundant lemma *)
  if filter_lemma selection lemma then begin
    if Config.print_lemmas selection.sel_config then begin
      print_endline ("REDUNDANT LEMMA: " ^ Term.clause_to_string lemma);
      (*    print_endline ("PRUNED LEMMA: " ^ String.concat "\n" (List.map Term.literal_to_string lemma));*)
    end;
  end

  (* add lemma *)
  else begin
    match Config.lemma selection.sel_config with
      | Flags.LM_Propositional ->
	  if Config.print_lemmas selection.sel_config then begin
	    print_endline ("LEMMA: " ^ Term.clause_to_string lemma);
	  end;
	  let global_lemma =
	    not selection.sel_bound#is_derivation_incomplete
	  in
	  begin
	    match selection.sel_lemmas_propositional with
	      | Some lemmas_propositional ->
		  Lemma_propositional.add_lemma lemmas_propositional lemma global_lemma
	      | None ->
		  failwith "Selection: lemmas propositional not created 2"
	  end
	      
      | Flags.LM_None
      | Flags.LM_Grounded
      | Flags.LM_Lifted ->
	  if Config.print_lemmas selection.sel_config then begin
	    print_endline ("LEMMA: " ^ Term.clause_to_string lemma);
	    (*    print_endline ("LEMMA: " ^ string_of_int (List.length lemma));*)
	    (*    print_endline ("LEMMA: " ^ String.concat "\n" (List.map Term.literal_to_string lemma));*)
	  end;
	  
	  prune_lemmas selection;
	  
	  (* create clause *)
	  let new_context_unifier_space =
	    Context_unifier_space.create_space
	      selection.sel_space_registry
	      selection.sel_problem_literals
	      lemma
	      selection.sel_process_close
	      selection.sel_process_assert
	      selection.sel_process_split
	      selection.sel_is_element_incomplete_assert
	      selection.sel_is_element_incomplete_split
	      true
	      selection.sel_finite_domain
	  in      
	    (* initially give the new lemma the worst existing utility value instead of 0,
	       that might be too unfair. *)
	    new_context_unifier_space.Context_unifier.cus_utility := (get_min_lemma_utility selection);
	    
	    let global_lemma =
	      not selection.sel_bound#is_derivation_incomplete
	    in
	      selection.sel_lemma_spaces <- (new_context_unifier_space, global_lemma) :: selection.sel_lemma_spaces;
	      
	      (* is this a unit lemma? *)
	      begin
		(* don't check original clause, but clause without constraints,
		   otherwise we miss this assert. *)
		match new_context_unifier_space.Context_unifier.cus_clause with
		  | unit_literal :: [] ->
		      (* check if contradictory - if so, close *)
		      let solutions =
			match new_context_unifier_space.Context_unifier.cus_constraints with
			  | None -> (Subst.empty, None) :: []
			  | Some constraints ->
			      (* unit assert, so empty substitution *)
			      Finite_domain_constraints.get_solutions
				(Finite_domain_constraints.setup constraints Subst.empty)
		      in
			List.iter
			  (fun (subst, constraints) ->
			     let unit_literal' =
			       Subst.apply_to_literal subst unit_literal Subst.input_literal_offset
			     in
			       match Context.check_contradictory selection.sel_context unit_literal' with
				 | Some element ->
				     (* close *)
				     let context_partner =
				       Context_unifier.create_context_partner
					 element None true
				     in
				     let raw_context_unifier =
				       Context_unifier.create_context_unifier
					 new_context_unifier_space [| context_partner |] false constraints
				     in
				       selection.sel_best_closing := Some raw_context_unifier;
				       raise_close selection
				  
					 
				 | None ->
				     (* unit assert *)
				     let raw_context_unifier =
				       Context_unifier.create_context_unifier
					 new_context_unifier_space [| Context_unifier.assert_partner |] false
					 constraints
				     in
				       Statistic.inc_computed_assert_candidates selection.sel_statistic;
				       selection.sel_unit_asserts <- (unit_literal', raw_context_unifier)
				       :: selection.sel_unit_asserts
			  )
			  solutions
		      
		  | _ ->
		      ()
	      end;
      
  end
  with
    | Lemma.TAUTOLOGY ->
	(* ignore tautologies -
	   can occure when lemmas are 'simplified', i.e. factored too much *)
	()





(*** selection ***)

(* check if the selected split candidate matches the guiding path *)
let check_replayed_decision (selected: selected) (guiding_step : guiding_step option) : unit =
  if not Const.debug then
    ()

  else if
    selected.Selection_types.candidate_type = State.Propagation
  then
    ()

  else begin
    match guiding_step with
      | None ->
	  ()

      | Some (Jumping.Split saved_literal) ->
	  (* if a left split, then it must be a recorded on *)
	  if
	    selected.Selection_types.candidate_type = (*Selection_types*)State.SplitLeft
	    &&
	    Term.are_literals_skolem_variants selected.Selection_types.literal saved_literal
	  then
	    ()
	    
	  else begin
	    print_endline ("SAVED: " ^ Term.literal_to_string saved_literal);
	    print_endline ("FOUND: " ^ Term.literal_to_string selected.Selection_types.literal);
	    failwith "Selection.check_replayed_decision 1"
	  end

	    
      | Some (Jumping.Left saved_literal)
      | Some (Jumping.Right saved_literal) ->
	  (* here there must be a right split *)
	  if
	    selected.Selection_types.candidate_type = (*Selection_types*)State.SplitRight
	    &&
	    Term.are_literals_skolem_variants selected.Selection_types.literal saved_literal
	  then
	    ()
	    
	  else begin
	    print_endline ("SAVED: " ^ Term.literal_to_string saved_literal);
	    print_endline ("FOUND: " ^ Term.literal_to_string selected.Selection_types.literal);
	    failwith "Selection.check_replayed_decision 2"
	  end
  end



(* pick a unit assert candidate, if an unapplied one is left *)
let rec select_unit_assert
    (selection: selection) (candidates: (literal * raw_context_unifier) list)
    : selected option =
  
  match candidates with
    | [] ->
	selection.sel_unit_asserts <- [];
	None
	
    | (candidate, raw_context_unifier) :: tail ->
	begin
	  match Context.check_subsumed selection.sel_context candidate with
	    | Some _ ->
		(* just forget a non-applicable candidate *)
		select_unit_assert selection tail
		
	    | None ->
		(* apply the assert *)
		selection.sel_unit_asserts <- tail;
		
		let subst =
		  Context_unifier.recompute_unifier ~recompute:true raw_context_unifier
		in
		State.apply_propagation
		  selection.sel_state
		  candidate
		  subst
		  raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_clause
		  (Context_unifier.get_constrained_clause raw_context_unifier)
		  raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_vars
		  0
		  (Context_unifier.create_space_utility_inc raw_context_unifier.Context_unifier.rcu_space)
		  [| |] (* no dependency *);
		
		Some {
		  Selection_types.candidate_type = State.Propagation;
		  Selection_types.literal = candidate;
		  Selection_types.raw_context_unifier = raw_context_unifier;
		}
	end






(* select a candidate literal *)
let select (selection: selection) : selected =

  (* first assert candidates leading immediately to a closing unifier. *)
  if Selection_assert.has_closing_candidate selection.sel_assert then begin
    match Selection_assert.select selection.sel_assert with
      | Some selected ->
	  selected
	    
      | None ->
	  failwith "Selection.select"
  end
    
  else
  (* then unit asserts *)
	  begin
	    match select_unit_assert selection selection.sel_unit_asserts with
	      | Some selected ->
		  selected
		    
	      | None ->
		  
		  (* then standard asserts *)
		  begin
		    match Selection_assert.select selection.sel_assert with
		      | Some selected ->
			  selected
			      
		      | None ->
	  
  (* then right splits *)
 begin
    match Selection_split.select_right_split selection.sel_split with
      | Some selected ->
	  selected
	    
      | None ->

				  (* then left splits *)
			  
			  (* recompute deferred split candidates, if necessary *)
			  if selection.sel_no_splits_yet then begin
			    selection.sel_no_splits_yet <- false;
			    Problem_literals.compute_deferred
			      selection.sel_problem_literals Context_unifier.Split;
			  end;
			    
			  begin
			    (* still a branch to replay? *)
			    let replay =
			      match selection.sel_guiding_path with
				| [] ->
				    None
				      
				| head :: tail ->
				    selection.sel_guiding_path <- tail;
				    Some head
			    in
			      (* finally try to find a split literal *)
			      match Selection_split.select selection.sel_split replay with
				| Some selected ->
				    check_replayed_decision selected replay;
				    selected
				      
				| None ->
				    (* process kept candidates exceeding the depth limit
				       to check if the branch is incomplete. *)
				    if
				      Config.is_horn selection.sel_config
				      ||
				      selection.sel_no_splits_yet
				    then
				      Selection_assert.check_exceeding selection.sel_assert
				    else
				      Selection_split.check_exceeding selection.sel_split;

				    raise SATISFIABLE
			  end
		  end  
	  end
  end









(*** Misc ***)

let get_lemmas (selection: selection) : clause list =
  begin
    match Config.lemma selection.sel_config with
      | Flags.LM_Propositional ->
	  begin
	    match selection.sel_lemmas_propositional with
	      | Some lemmas_propositional ->
		  Lemma_propositional.get_lemmas lemmas_propositional;
	      | None ->
		  failwith "Selection: lemmas propositional not created 3"
	  end

      | Flags.LM_None
      | Flags.LM_Grounded
      | Flags.LM_Lifted ->
  List.rev (
  List.fold_left
    (fun acc (space, global) ->
       if global then
	 let clause =
	   match space.Context_unifier.cus_constraints with
	     | Some constraints ->
		 Finite_domain_constraints.get_clause constraints 
	     | None ->
		 space.Context_unifier.cus_clause
	 in
	   clause :: acc
       else
	 acc
    )
    []
    selection.sel_lemma_spaces
  )
(*
  List.map
    (fun space ->
       space.Context_unifier.cus_clause
    )
    selection.sel_lemma_spaces
*)
  end




let in_replay (selection: selection) : bool =
  selection.sel_guiding_path != []





(*** backtrack ***)

(* decrease the utility of clauses *)
let decay_clause_utility (selection: selection) : unit =
  (* update counter *)
  selection.sel_decay_counter :=
    (!(selection.sel_decay_counter) + 1) mod Const.decay_clause_utility_interval;

  (* decay utilities if counter is 0 *)
  if !(selection.sel_decay_counter) = 0 then begin
    Array.iter
      (fun space ->
	 space.Context_unifier.cus_utility :=
	   !(space.Context_unifier.cus_utility) / Const.decay_clause_utility_ratio
      )
      selection.sel_clause_spaces;

    List.iter
      (fun (space, _) ->
	 space.Context_unifier.cus_utility :=
	   !(space.Context_unifier.cus_utility) / Const.decay_clause_utility_ratio
      )
      selection.sel_lemma_spaces
  end






let backtrack (selection: selection) (retracted: choice_point) (explanation: literal list) : unit =
(*  decay_clause_utility selection;*)

  (* backtracking invalidates everything from the last split on,
     so in particular all asserted literals are retracted. *)

  selection.sel_best_closing := None;

  Problem_literals.backtrack selection.sel_problem_literals;

  (* partial context unifiers - dependent on Problem_literals.backtrack
     backtrack only not subsumed clauses to avoid jumping through memory.
     for lemmas this showed to be potentially expensive.
  *)
  Array.iter
    (fun space ->
       if Problem_literals.is_space_subsumed selection.sel_problem_literals space then
	 ()
       else
	 Context_unifier_space.backtrack space
    )
    selection.sel_clause_spaces;

  List.iter
    (fun (space, _) ->
       if Problem_literals.is_space_subsumed selection.sel_problem_literals space then
	 ()
       else
	 Context_unifier_space.backtrack space
    )
    selection.sel_lemma_spaces;


  (* candidates *)
  Selection_assert.backtrack selection.sel_assert;
  Selection_split.backtrack selection.sel_split retracted explanation;
  
  begin
    match Config.lemma selection.sel_config with
      | Flags.LM_Propositional ->
	  begin
	    match selection.sel_lemmas_propositional with
	      | Some lemmas_propositional ->
		  Lemma_propositional.backtrack lemmas_propositional;
	      | None ->
		  failwith "Selection: lemmas propositional not created 4"
	  end

      | Flags.LM_None
      | Flags.LM_Grounded
      | Flags.LM_Lifted ->
	  ()
  end



(*
  (* get the split literals of the explanation *)
  match context_unifier with
    | None ->
	()

    | Some context_unifier ->
  let literals =
    Context_unifier.get_context_literals context_unifier.Context_unifier.rcu_context_partners
  in

  let lemma =
    get_explanation_ selection.sel_state selection.sel_context literals
  in
(*  let lemma =
    List.map State.left_split_literal (retracted :: explanation)
      ROOT ASSERTS!!!
  in*)
    if not (filter_lemma selection lemma) then begin
      let global_lemma =
	not (Bound.is_derivation_incomplete selection.sel_bound)
      in
	Lemma_propositional.add_lemma selection.sel_lemmas_propositional lemma global_lemma
    end


*)
