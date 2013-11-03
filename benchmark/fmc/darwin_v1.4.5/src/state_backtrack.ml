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



(*** types ***)

type config = Config.config
type state = State.state
type context = Context.context
type literal = Term.literal
type clause = Term.clause
type choice_point = State.choice_point
type explanation = State.explanation
type branch_node = State.literal_info
type raw_context_unifier = Context_unifier.raw_context_unifier


exception UNSATISFIABLE = Context_unifier.UNSATISFIABLE



(*** scoring ***)




let rec score' (state: state) (scored: clause list) (to_score: literal list) : unit =
  match to_score with
    | [] ->
	()

    | context_literal :: tail ->
	let literal_info =
	  try
	    State.get_literal_info state context_literal
	  with
	    | Not_found ->
		failwith ("State_backtrack: score' " ^ Term.literal_to_string context_literal)
	in

	  (* score each clause only once *)
	  if List.memq literal_info.State.li_clause scored then begin
	    score' state scored tail
	  end

	  else begin
	    (* score clause *)
	    literal_info.State.li_clause_utility ();

            (* add the context literals to be scored as well.
	       for a split no context literals are given, so scoring is stopped automatically.  *)
            let rec score_add' index to_score =
	      if index >= Array.length literal_info.State.li_context_literals then
		to_score

	      else
		let context_literal =
		  literal_info.State.li_context_literals.(index)
		in
		  (* ignore pseudo context literals *)
		  if
		    Term.literal_equal context_literal Term.assert_literal
		    ||
		    Term.literal_equal context_literal Term.minus_v
		    ||
		    Term.literal_equal context_literal Term.plus_v
		  then
		    score_add' (index + 1) to_score

		  else
		    score_add' (index + 1) (context_literal :: to_score)
	    in
	    let to_score' =
	      score_add' 0 to_score
	    in
	      score' state (literal_info.State.li_clause :: scored) to_score'

	  end
  

let score (state: state) (closing_context_unifier: raw_context_unifier) : unit =
  let clause =
    closing_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_clause
  in
  let context_literals =
    Context_unifier.get_context_literals
      closing_context_unifier.Context_unifier.rcu_context_partners
  in

    (* score closing clause *)
    closing_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_utility :=
      !(closing_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_utility) + 1;
	
    (* score all other involved clauses *)
    score' state [clause] (Array.to_list context_literals)



(*** backtracking ***)


(* invalidates and removes from the derivation branch
   all choice points older than and including the choice point to retract. *)
let invalidate (state: state) (retract: choice_point) : unit =

  let rec invalidate' choice_points =
    match choice_points with
      | head :: tail ->
	  if State.compare_age retract head < 0 then begin
	    State.invalidate_choice_point head;
	    invalidate' tail
	  end

	  else if State.compare_age retract head = 0 then begin
	    State.invalidate_choice_point head;
	    State.set_branch state tail;
	  end

	  else begin
	    failwith "State_backtrack.invalidate 1"
	  end
	      
      | [] ->
	  failwith "State_backtrack.invalidate 2"
  in
    invalidate' (State.get_branch state)




module To_regress =
  Set.Make (
    struct
      type t = (int * State.literal_info)

      let compare (x, _) (y, _) =
	Tools.compare_int x y
	    
    end
  )

let rec get_explanation_ (state: state) context (literals: literal array) : literal list =
  let add_to_regress to_regress literals =
    Array.fold_left
      (fun acc literal ->
	 if Term.literal_equal literal Term.assert_literal then
	   acc

	 else begin
	 let literal_info =
	   try
	     State.get_literal_info state literal
	     (*LiteralTable.find state.st_literal_info literal*)
	   with
	     | Not_found ->
		 failwith ("State.get_explanation_ 1: " ^ Term.literal_to_string literal)
	 in
	 let id =
	   match Context.element_for_literal context literal with
	     | None ->
		 failwith ("State.get_explanation_ 2: " ^ Term.literal_to_string literal)

	     | Some element ->
		 element.Context.el_id
	 in
	   To_regress.add (id, literal_info) acc
	 end
      )
      to_regress
      literals
  in

  let to_regress =
    add_to_regress To_regress.empty literals
  in

  let rec regress regressed to_regress =
    if To_regress.is_empty to_regress then
      regressed

    else begin
      let element =
	To_regress.max_elt to_regress
      in
      let to_regress' =
	To_regress.remove element to_regress
      in
      let _, literal_info =
	element
      in
	(* split or root assert *)
	if Array.length literal_info.State.li_context_literals = 0 then begin
	  match literal_info.State.li_type with
	    | State.SplitLeft ->
		(* split  *)
		if
		  Term.literal_equal literal_info.State.li_literal Term.minus_v
		  ||
		  Term.literal_equal literal_info.State.li_literal Term.plus_v
		then
		  (* ignore -v *)
		  failwith "minus v"
		else
		  (* add split literal to regressed set *)
		  regress (literal_info.State.li_literal :: regressed) to_regress'
		
	    | State.SplitRight
	    | State.Propagation ->
		(* ignore root assert or root right split *)
		regress regressed to_regress'
	end

	(* assert or unit split *)
	else
	  regress regressed (add_to_regress to_regress' literal_info.State.li_context_literals)
    end
  in
    regress [] to_regress


let lemma_count = ref 0


(* backtrack state based on the closing context unifier *)
let backtrack (state: state) (context: context) (config: config) (closing_context_unifier: raw_context_unifier) (*(domain_constants: Term.term list)*) :
    choice_point * literal list(*explanation*) * clause option
    =
  let clause =
    closing_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_clause
  in
  let context_literals =
    Context_unifier.get_context_literals
      closing_context_unifier.Context_unifier.rcu_context_partners
  in

  let regression_explanation =
    State.get_explanation state context_literals
  in

  let literal_explanation =
    List.map
      State.left_split_literal
      regression_explanation
  in

    if Const.debug then begin
      let explanation =
	regression_explanation
      in
      let explanation' =
	State.get_explanation state (Array.of_list literal_explanation)
      in
	
	if not (Tools.lists_equal State.choice_point_equal explanation explanation') then begin
	  print_endline (State.explanation_to_string explanation);
	  print_endline (State.explanation_to_string explanation');
	  failwith ("Not closing explanation 1!")
	end;
	
    end;

    let closing_explanation  =
      match Config.backtracking config with
	| Flags.BT_Naive ->
	    State.get_branch state
	      
	| Flags.BT_Backjumping ->
	    regression_explanation
    in



    (* scoring must be done before the used dependency information is backtracked *)
    score state closing_context_unifier;

  let retract, _retract_next, _right_explanation =
    match closing_explanation with
      | [] ->
	  raise UNSATISFIABLE
	  
      | root :: [] ->
	  if
	    State.choice_point_equal (State. root_choice_point state) root
	    ||
	    State.choice_point_equal State.valid_choice_point root
	  then
	    raise UNSATISFIABLE
	  else
	    root, (State.root_choice_point state), []

      | retract :: retract_next :: tail ->
	  retract, retract_next, (retract_next :: tail)
  in

    if false && Const.debug && not (List.memq (State.left_split_literal retract) literal_explanation) then begin
      print_endline (Term.literal_to_string (State.left_split_literal retract));
      print_endline (Term.clause_to_string literal_explanation);
      failwith ("Not closing explanation 2!")
    end;

  let backjump =
    (* how much do we actually want to backtrack?
       we could go right to the backjump target, like SAT solvers do.
       but in our case that mostly leads to the recomputation
       of most of the jumped over derivation, and thus a performance loss.
       therefore, just retract the most recent responsible left split. *)
    retract;
    (*
    let rec find_backjump branch =
      match branch with
       | [] ->
	   failwith "State_backtrack.find_backjump 0"
	     
       | _ :: [] ->
	   failwith "State_backtrack.find_backjump 1"
	     
       | x :: y :: tail ->
	   if State.choice_point_equal y retract_next then
	     x
	   else
	     find_backjump (y :: tail)
    in
      find_backjump (State.get_branch state)
    *)
  in


  (* lemma must be computed before the used dependency information is backtracked *)
  let lemma =
    match Config.lemma config with
      | Flags.LM_None ->
	  None

      | Flags.LM_Grounded ->
	  Lemma_grounded.get_lemma ~uip:(Config.lemma_uip config) state context
	    (Context_unifier.recompute_full_unifier ~recompute:true closing_context_unifier)
	    clause context_literals retract

      | Flags.LM_Lifted ->
	  let constrained =
	    Context_unifier.get_constrained_clause closing_context_unifier
	  in
	  Lemma_lifted.get_lemma ~uip:(Config.lemma_uip config) state context
	  (Context_unifier.recompute_full_unifier ~recompute:true closing_context_unifier)
	  clause constrained context_literals retract

      | Flags.LM_Propositional ->
	    Some (
	      List.map
		(fun literal ->
		   Term.request_negated_literal literal
		)
	      literal_explanation
	    )
  in


    invalidate state backjump;

    (* remove invalidated context literals. *)
    State.backtrack_literal_info state;

    (*retract, right_explanation, lemma*)
    retract, literal_explanation, lemma


let backtrack_incomplete (state: state) (retract: choice_point) : unit =
  if
    State.choice_point_equal (State.root_choice_point state) retract
    ||
    State.choice_point_equal State.valid_choice_point retract
  then begin
    raise UNSATISFIABLE
  end;
  invalidate state retract;

  (* remove invalidated context literals. *)
  State.backtrack_literal_info state
