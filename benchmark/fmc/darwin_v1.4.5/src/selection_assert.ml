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


type config = Config.config
type bound = Bound.bound
type statistic = Statistic.statistic
type symbol = Symbol.symbol
type literal = Term.literal
type choice_point = State.choice_point
type state = State.state
type raw_context_unifier = Context_unifier.raw_context_unifier
type problem_literals = Problem_literals.problem_literals
type selected = Selection_types.selected
type selection_lookahead = Selection_lookahead.selection_lookahead
type context = Context.context




type candidate = {
  (* the context unifier. *)
  ca_raw_context_unifier: raw_context_unifier;

  (* depth or weight, depending on the restart bound method. *)
  ca_term_quality: int;

  (* see Context_unifier.generation_of_candidate *)
  ca_generation: int;

  ca_sort : Symbol.sort;
}



(*** comparison ***)

(* :TODO: add clause utility *)
let compare_candidates ~(different: bool) (first: candidate) (second: candidate) : int =
  let con =
    match first.ca_sort, second.ca_sort with
      | Symbol.Connection, Symbol.Connection -> 0
      | Symbol.Connection, _ -> 1
      | _, Symbol.Connection -> -1
      | _ -> 0
  in
    if con <> 0 then
      con
    else begin

  if first.ca_term_quality < second.ca_term_quality then
    -1
  else if first.ca_term_quality > second.ca_term_quality then
    1
  else begin
(*    
      (* clause utility *)
      let cmp =
	Tools.compare_int
	  !(second.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_utility)
	  !(first.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_utility)
      in
	if cmp <> 0 then
	  cmp
	else begin
    if
      first.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_lemma
      &&
	not second.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_lemma
    then
      -1
    else if
      not first.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_lemma
      &&
	second.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_lemma
    then
      1
    else*)
	  begin
	    if first.ca_generation < second.ca_generation then
	      -1
	    else if first.ca_generation > second.ca_generation then
	      1
	    else
	      begin
		Context_unifier.compare_context_unifiers ~different:different
		  first.ca_raw_context_unifier second.ca_raw_context_unifier
	      end
	  end
(*	end*)
  end
  end

let candidate_to_string title candidate =
  let context_element : Context.element =
    Context_unifier.creating_context_element candidate.ca_raw_context_unifier
  in    
    (title ^ Term.literal_to_string context_element.Context.el_literal)
    ^ "\n"
    ^ (Context_unifier.raw_context_unifier_to_string candidate.ca_raw_context_unifier)
    ^ "\n"
    


(* cache for potentially applicable candidates *)
module Valid =
  Heap.MinMaxHeap (
    struct
      type t = candidate

      let compare = compare_candidates ~different:Const.stable_derivation
      let to_string = candidate_to_string ""
    end
  )

(* for potentially applicable but forgotten candidates *)
(* access by candidate ordering *)
module ForgottenOrder =
  Set.Make (
    struct
      type t = (Context.element * candidate)

      let compare (_, candidate0) (_, candidate1) =
	  compare_candidates ~different:Const.stable_derivation candidate0 candidate1
    end
  )

(* access by context element *)
module ForgottenBarrier =
  Hashtbl.Make (
    struct
      type t = Context.element

      let equal = Context.element_equal

      let hash element =
	element.Context.el_id
    end
  )


type candidates = {
  (* environment *)
  cd_config: config;
  cd_bound: bound;
  cd_statistic: statistic;
  cd_state: state;
  cd_context: context;
  cd_problem_literals: problem_literals;

  (* candidates not yet considered *)
  cd_unprocessed: raw_context_unifier array;
  (* number of valid entries *)
  mutable cd_unprocessed_size: int;

  (* the candidates currently valid for adding to the context
     (at least valid when last checked). *)
  cd_valid: Valid.t;

  (* forgotten manages the candidates that don't fit into cd_valid,
     but still have to be processed for completeness.
     the best forgotten candidate for each context element
     is called its 'barrier'. *)


  (*** forgotten ***)

  (* ordering over the barriers *)
  mutable cd_forgotten_order: ForgottenOrder.t;

  (* mapping from context element to its barrier *)
  cd_forgotten_barrier: candidate ForgottenBarrier.t;

  (* if the forgotten candidates of an element are to be recomputed,
     its former barrier is stored here while the recomputation takes place.
     i.e., all recomputed candidates below this barrier must be ignored. *)
  mutable cd_forgotten_recomputation_barrier: (Context.element * candidate) option;



  (*** exceeding ***)

  (* hash table for context elements by their id.

     for these context literals exceeding applicable candidates
     have been found and dropped during their initial computation.
     that is, when checking for a complete exhausted branch
     these have to be recomputed and rechecked. *)
  cd_exceeding_elements: (int, Context.element) Hashtbl.t;

  (* flag to denote that currently candidates are recomputed
     just to be checked for finding applicable exceeding ones. *)
  mutable cd_check_exceeding: bool;


  (*** lookahead ***)

  (* lookahead for contradictions within assert candidates *)
  mutable cd_lookahead: selection_lookahead;

  (* lookahead for contradictions within assert candidates
     exceeding the deepening bound. *)
  cd_lookahead_exceeding: selection_lookahead;

  (* if lookahead checking finds a candidate leading to a closing context unifier
     it is stored here:
     candidate * assert_literal * index of assert_literal in context_partners. *)
  mutable cd_preselected: (candidate * literal * int) option;
}


(* raised if the current checking of exceeding candidates
   already implies a restart with the next higher deepening bound.
   that is, further evaluation of exceeding candidates
   yields no further information and can be omitted. *)
exception EXCEEDING_EXHAUSTED


(*** representation ***)


let candidate_to_string_ (config: config) (state: state)
  (title: string) (candidate: candidate) (literal: literal) : string =

  State.active_choice_point_to_string state
  ^ title ^ ": " ^ Term.literal_to_string literal ^ "\n"
  ^
  if Config.print_derivation_context_unifier config then
    Context_unifier.raw_context_unifier_to_string candidate.ca_raw_context_unifier
  else
    ""



			      
(*** creation ***)

let null_candidate = {
  ca_raw_context_unifier = Context_unifier.null_context_unifier;
  ca_term_quality = -1;
  ca_generation = -1;
  ca_sort = Symbol.Skolem;
}

let create (config: config) (bound: bound) (statistic: statistic) (state: state)
  (context: context) (problem_literals: problem_literals) : candidates =
  (* min. 1 entry necessary for function add *)
  let unprocessed =
    Array.make (Tools.max_int 1 Const.max_unprocessed_assert_candidates) Context_unifier.null_context_unifier
  in
  {
    cd_config = config;
    cd_bound = bound;
    cd_statistic = statistic;
    cd_state = state;
    cd_context = context;
    cd_problem_literals = problem_literals;
    
    cd_unprocessed = unprocessed;
    cd_unprocessed_size = 0;

    cd_valid = Valid.create null_candidate;

    cd_forgotten_order = ForgottenOrder.empty;
    cd_forgotten_barrier = ForgottenBarrier.create 64;
    cd_forgotten_recomputation_barrier = None;

    cd_exceeding_elements = Hashtbl.create 64;
    cd_check_exceeding = false;

    cd_lookahead = Selection_lookahead.create Const.max_assert_lookahead;
    cd_lookahead_exceeding = Selection_lookahead.create Const.max_assert_lookahead_exceeding;
    cd_preselected = None;
  }






(*** Misc ***)

let has_closing_candidate (candidates: candidates) : bool =
  match candidates.cd_preselected with
    | None -> false
    | _ -> true


let size (candidates: candidates) : int =
  Valid.size candidates.cd_valid




(*** valid candidates ***)

(* remove all retractred candidates at the beginning of the valid candidate priority queue *)
let rec retract_valid_min (candidates: candidates) : unit =
  if Valid.is_empty candidates.cd_valid then begin
    ()
  end

  else begin
    let candidate =
      Valid.min candidates.cd_valid
    in
      (* candidate is no longer valid (due to backtracking) *)
      if Context_unifier.is_raw_context_unifier_invalid candidate.ca_raw_context_unifier then begin
	ignore (Valid.remove_min candidates.cd_valid: candidate);
	retract_valid_min candidates;
      end
  end

(* remove all retracted candidates at the beginning of the valid candidate priority queue *)
let rec retract_valid_max (candidates: candidates) : unit =
  if Valid.is_empty candidates.cd_valid then begin
    ()
  end

  else begin
    let candidate =
      Valid.max candidates.cd_valid
    in
      (* candidate is no longer valid (due to backtracking) *)
      if Context_unifier.is_raw_context_unifier_invalid candidate.ca_raw_context_unifier then begin
	ignore (Valid.remove_max candidates.cd_valid: candidate);
	retract_valid_max candidates;
      end
  end



(*** forgotten ***)


(* get the barrier of the given candidate,
   throws Not_found if none exists *)
let get_barrier (candidates: candidates) (candidate: candidate) : candidate =
  (* find the context element responsible for this candidate *)
  let context_element : Context.element =
    Context_unifier.creating_context_element candidate.ca_raw_context_unifier
  in
    ForgottenBarrier.find candidates.cd_forgotten_barrier context_element
	  

(* is this candidate part of the forgotten candidate set? *)
let is_candidate_forgotten (candidates: candidates) (candidate: candidate) : bool =
  try
    compare_candidates ~different:false (get_barrier candidates candidate) candidate <= 0
  with
    | Not_found ->
	(* nope, no forgotten candidate yet *)
	false


(* update the barrier of the forgotten candidates to include this one. *)
let update_forgotten (candidates: candidates) (forgotten: candidate) : unit =
  (* find the context element responsible for this candidate *)
  let context_element : Context.element =
    Context_unifier.creating_context_element forgotten.ca_raw_context_unifier
  in    
    try
      (* find its barrier *)
      let barrier : candidate =
	ForgottenBarrier.find candidates.cd_forgotten_barrier context_element
      in
	(* below the barrier, so update it *)
	if compare_candidates ~different:true barrier forgotten > 0 then begin
	  ForgottenBarrier.replace candidates.cd_forgotten_barrier context_element forgotten;
	  candidates.cd_forgotten_order <- ForgottenOrder.remove (context_element, barrier) candidates.cd_forgotten_order;
	  candidates.cd_forgotten_order <- ForgottenOrder.add (context_element, forgotten) candidates.cd_forgotten_order
	end
    with
      | Not_found ->
	  (* first forgotten candidate for this element, create a new barrier *)
	  ForgottenBarrier.add candidates.cd_forgotten_barrier context_element forgotten;
	  candidates.cd_forgotten_order <- ForgottenOrder.add (context_element, forgotten) candidates.cd_forgotten_order




(*** exceeding candidates ***)

(* have exceeding applicable candidates been dropped for this context literal? *)
let is_marked_exceeding (candidates: candidates) (context_element: Context.element) : bool =
  try
    ignore (Hashtbl.find candidates.cd_exceeding_elements context_element.Context.el_id: Context.element);
    true
  with
    | Not_found ->
	false

let is_element_incomplete (candidates: candidates) (element: Context.element) : bool =
  Config.restart candidates.cd_config = Flags.RS_Delayed
  &&
  not candidates.cd_check_exceeding
  &&
  is_marked_exceeding candidates element

(* an exceeding applicable candidate has been dropped for this context literal *)
let mark_exceeding (candidates: candidates) (context_element: Context.element) : unit =
  Hashtbl.add candidates.cd_exceeding_elements context_element.Context.el_id context_element





(*** build ***)


(* compute the quality of a term based on the deepening bound criterion. *)
let get_term_quality (candidates: candidates) (literal: literal) : int =
  if Config.iterative_deepening candidates.cd_config = Flags.IT_TermDepth then
    Term_attributes.weight_of_literal literal
  else
    Term_attributes.depth_of_literal literal


(* build the assert literal represented by an assert context unifier,
   return it and it's context partner index in the context unifier. *)
let build_assert_literal (raw_context_unifier: raw_context_unifier) : literal * int =
  (* find the assert literal input partner *)
  let input_partners =
    raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_input_partners
  in
  let assert_literal_partner, index =
    let rec check_at (index: int) : literal * int =
      if index >= Array.length input_partners then begin
	print_endline (Context_unifier.raw_context_unifier_to_string raw_context_unifier);
	failwith "Selection_assert.build_assert_literal"
      end

      else begin
	let input_partner =
	  input_partners.(index)
	in
	  if raw_context_unifier.Context_unifier.rcu_context_partners.(input_partner.Context_unifier.ip_index)
	    ==
	    Context_unifier.assert_partner
	  then
	    input_partner.Context_unifier.ip_literal, input_partner.Context_unifier.ip_index
	  else
	    check_at (index + 1)
      end
    in
      check_at 0
  in

  (* build the assert literal *)
  let subst =
    Context_unifier.recompute_unifier ~recompute:true raw_context_unifier
  in
  let assert_literal =
    Subst.apply_to_literal ~insert_db:false
      subst
      assert_literal_partner
      Subst.input_literal_offset
  in
    assert_literal, index


(* build a candidate data structure *)
let build_candidate' (candidates: candidates) (raw_context_unifier: raw_context_unifier)
    (assert_literal: literal) : candidate =
  {
    ca_raw_context_unifier = raw_context_unifier;
    ca_term_quality = get_term_quality candidates assert_literal;
    ca_generation = Context_unifier.generation_of_candidate raw_context_unifier.Context_unifier.rcu_context_partners;
    ca_sort = Term.get_literal_sort assert_literal;
  }



(* is this candidate / literal conflicting with the context or lookahead? *)
let is_candidate_conflicting (candidates: candidates) (assert_literal: literal) : bool =
  (* conflicting with the lookahead? *)
  Selection_lookahead.check candidates.cd_lookahead assert_literal 
  ||
  (* conflicting with the exceeding lookahead? *)
  Selection_lookahead.check candidates.cd_lookahead_exceeding assert_literal

  ||
  (* conflicting with the context?

     early check for contradictory assert candidates.
     later on the corresponding closing context unifier is computed anyway,
     but this way computation of other assert and split context unifiers
     is stopped early.

     TODO: does this really pay off?
  *)
  (
    match Context.check_contradictory candidates.cd_context assert_literal with
      | Some _ -> true
      | None -> false
  )


(* register a candidate to the close lookahead set.
   returns false if the candidate has been dropped as a duplicate of a registered candidate. *)
let register_to_lookahead (candidates: candidates)
    (raw_context_unifier: raw_context_unifier) (assert_literal: literal)
    : unit =

  (* exceeding? *)
  if raw_context_unifier.Context_unifier.rcu_exceeding_complexity then begin
    (* store exceeding candidates? *)
    if Config.lookahead_exceeding candidates.cd_config then begin
      Selection_lookahead.add ~no_duplicates:true
	candidates.cd_lookahead_exceeding assert_literal raw_context_unifier
    end
  end

  else
    (* regular candidate *)
    Selection_lookahead.add ~no_duplicates:true
      candidates.cd_lookahead assert_literal raw_context_unifier



let add_valid (candidates: candidates) (candidate: candidate) : unit =
  try
    (* there is space left for this candidate, so just add it. *)
    if Valid.size candidates.cd_valid < Const.max_assert_candidates then begin
      Valid.add candidates.cd_valid candidate;
    end
    
    (* this candidate is worse than all existing ones, so forget it *)
    else if compare_candidates ~different:true candidate (Valid.max candidates.cd_valid) > 0 then begin
      update_forgotten candidates candidate
    end

    (* this candidate is
       - better than the worst valid one
       - but worse than its barrier
       this can only happen if the candidate is reactivated after backtracking,
       but that can not happen for assert candidates.
    *)
    else if Const.debug && is_candidate_forgotten candidates candidate then begin
      failwith "Selection_assert.forgotten invalid";
    end

    (* forget the worst valid candidate and add this one instead *)
    else begin
      let forgotten =
	Valid.remove_max candidates.cd_valid
      in
	(* prune queue eagerly. *)
	retract_valid_max candidates;

	update_forgotten candidates forgotten;
	Valid.add candidates.cd_valid candidate;
    end
  with
    | Heap.OVERFLOW ->
	raise (Const.NO_SOLUTION "Selection_assert.add_valid: candidate set overflow")


(* builds the assert candidate represented by an assert context unifier
   and adds it to the set of valid candidates.

   checks candidates for applicability and drops non-applicable ones.

   different strategies:
   - immediately drop exceeding candidates
   - register exceeding candidates in lookahead
   - mark exceeding candidates for later checking for an exhausted branch

   returns the candidate, if it is lookahead closing.
*)
let build_candidate (candidates: candidates) (raw_context_unifier: raw_context_unifier) :
    (candidate * literal * int) option =
  let assert_literal, index =
    build_assert_literal raw_context_unifier
  in
  let candidate =
    build_candidate' candidates raw_context_unifier assert_literal
  in
  let complexity =
    candidates.cd_bound#get_complexity assert_literal
  in
    (* :TODO: NAC_lookahead *)
    (* are we recomputing forgotten candidates? *)
    match candidates.cd_forgotten_recomputation_barrier with
      | Some (_, forgotten_barrier) when
	  (* ignore candidates below the recomputation threshhold *)
	  compare_candidates ~different:false candidate forgotten_barrier < 0
	  ||
	  (* ignore candidates exceeding the deepending bound - they have been stored before *)
	  candidates.cd_bound#exceeds complexity
	  ->
	None
	  
      | _ ->
	  (* are we checking exceeding candidates for applicability? *)
	  if candidates.cd_check_exceeding then begin
	    (* negative assert candidates to be ignored. *)
	    if
	      Config.neg_assert_candidates candidates.cd_config != Flags.NAC_Use
	      &&
	      not assert_literal.Term.sign
	    then
	      ()

	    (* check this candidate for applicability *)
	    else begin
	      let choice_point =
		Context_unifier.get_creation_choice_point raw_context_unifier.Context_unifier.rcu_context_partners
	      in
		(* would the candidate change the increased depth bound after a restart?
		   as the candidates are recomputed in chronological context literal order
		   the earliest incomplete choice point is automatically found.
		*)
		if candidates.cd_bound#exceeds_current complexity choice_point then begin
		
		  (* ... and is it applicable? *)
		  match Context.check_subsumed candidates.cd_context assert_literal with
		    | Some _ -> ()
		    | None ->
			(* yes, so register it *)
			ignore (
			  candidates.cd_bound#register complexity choice_point
			    : bool);
			
			if candidates.cd_bound#has_min_exceeding then
			  raise EXCEEDING_EXHAUSTED
		end
	    end;
	    
	    None
	  end

	  (* is this a negative candidate meant for lookahead only? *)
	  else if
	    Config.neg_assert_candidates candidates.cd_config = Flags.NAC_Lookahead
	    &&
	    not assert_literal.Term.sign
	  then begin
	    if is_candidate_conflicting candidates assert_literal then
	      Some (build_candidate' candidates raw_context_unifier assert_literal, assert_literal, index)
		
	    else begin
	      register_to_lookahead candidates raw_context_unifier assert_literal;
	      None
	    end
	  end
	      
	  (* is this candidate exceeding the current bound? *)
	  else if candidates.cd_bound#exceeds complexity then begin
	    (* exceeding candidate for lookahead *)
	    if
	      Config.lookahead_exceeding candidates.cd_config
	      &&
	      is_candidate_conflicting candidates assert_literal
	    then
	      Some (build_candidate' candidates raw_context_unifier assert_literal, assert_literal, index)

	    else begin
	      (* lookahead registration *)
	      register_to_lookahead candidates raw_context_unifier assert_literal;

	      (* check if delayed processing *)
	      begin
		match Config.restart candidates.cd_config with
		  | Flags.RS_Eager
		  | Flags.RS_Lazy ->
		      (* no, drop the candidate *)
		      ()
			
		  | Flags.RS_Delayed ->
		      let context_element =
			Context_unifier.creating_context_element raw_context_unifier
		      in
			(* for this context literal exceeding applicable candidates
			   have already been dropped. *)
			if is_marked_exceeding candidates context_element then
			  ()
			    
			(* is this the first applicable exceeding candidate to drop
			   for this context literal? *)
			else begin
			  (* is the candidate applicable?*)
			  match Context.check_subsumed candidates.cd_context assert_literal with
			    | Some _ ->
				(* no, not applicable, so just drop it. *)
				()
				  
			    | _ ->
				mark_exceeding candidates context_element;
			end
	      end;
	      
	      None
	    end
	  end
	      
	  (* a regular candidate (obeying the depth bound) *)
	  else begin
	    (* is the candidate applicable?*)
	    match Context.check_subsumed candidates.cd_context assert_literal with
	      | Some _ ->
		  (* no, not applicable, so just drop it.
		     as asserts are applied eagerly the subsuming candidate literal
		     will exist as long as this candidate exists anyway. *)
		  None
		    
	      | _ ->
		  (* yes, applicable *)
		  
		  (* lookahead check *)
		  if is_candidate_conflicting candidates assert_literal then begin
		    Some (candidate, assert_literal, index)
		  end
		    
		  else begin
		    (* no, fresh candidate *)
		    if Config.print_assert_candidates candidates.cd_config then begin
		      print_string (
			candidate_to_string_
		      candidates.cd_config candidates.cd_state
			  "New Assert Candidate" candidate assert_literal);
		    end;
		    
		    register_to_lookahead candidates raw_context_unifier assert_literal;
		    add_valid candidates candidate;

		    None		    
		  end;		  
	  end





(*** add ***)


(* empty buffer of unprocessed candidates *)
let clear_unprocessed (candidates: candidates) : unit =
  for i = 0 to candidates.cd_unprocessed_size - 1 do
    candidates.cd_unprocessed.(i) <- Context_unifier.null_context_unifier;
  done;
  candidates.cd_unprocessed_size <- 0


(* returns true if a lookahead closing candidate is known.
   only processes the candidates and empties the buffer
   if the buffer is full or force is true.
*)
let build_unprocessed ~(force: bool) (candidates: candidates) : bool =
  (* buffer not full, do nothing *)
  if
    not force
    &&
    candidates.cd_unprocessed_size < Array.length candidates.cd_unprocessed - 1
  then begin
    false
  end

  (* build and check all new candidates *)
  else begin
    match candidates.cd_preselected with
      | Some _
	  (* if not stable we just stop computing assert context unifiers
	     as soon as we know that we can close *)
	  when not Const.stable_derivation ->
	  (* just ignore the new candidates, we know we can close without them. *)
	  clear_unprocessed candidates;
	  true
	  
      | _ ->
	  let rec build_at (index: int) : bool =
	    if index >= candidates.cd_unprocessed_size then begin
	      (* all build, none closing found *)
	      false
	    end

	    else begin
	      (* build this candidate *)
	      let candidate =
		candidates.cd_unprocessed.(index)
	      in
		match build_candidate candidates candidate with
		  | None ->
		      build_at (index + 1)
			
		  | Some _ as conflicting ->
		      (* great, can be used to close, stop *)

		      (* ensure that if several closing candidates are found
			 the same one is always used to close,
			 independ of the actual computation order.
			 just done to produce the same derivation.
		      *)
		      begin
			match candidates.cd_preselected with
			  | Some (old_candidate, _, _) when
			      Context_unifier.compare_context_unifiers
				~different:true
				old_candidate.ca_raw_context_unifier candidate
			      <= 0 ->
			    ()

			  | _ ->
			      candidates.cd_preselected <- conflicting;
		      end;

		      true
	    end
	  in

	  (* build the new candidates *)
	  let closing_found =
	    build_at 0
	  in
	    clear_unprocessed candidates;
	    closing_found
  end


let add (candidates: candidates) (candidate: raw_context_unifier) : bool =
  Statistic.inc_computed_assert_candidates candidates.cd_statistic;

  (* store the new candidate for later processing *)
  candidates.cd_unprocessed.(candidates.cd_unprocessed_size) <- candidate;
  candidates.cd_unprocessed_size <- candidates.cd_unprocessed_size + 1;

  build_unprocessed ~force:false candidates




(*** select ***)




(* if there is a forgotten candidate which is better than the best valid one,
   rebuild all its elements forgotten candidates. *)
let rec rebuild_forgotten (candidates: candidates) : unit =
  retract_valid_min candidates;

  (* no forgotten candidates *)
  if ForgottenOrder.is_empty candidates.cd_forgotten_order then
    ()

  else begin
    let (forgotten_element, forgotten_barrier) =
      ForgottenOrder.min_elt candidates.cd_forgotten_order
    in
      if
	(* no cached elements left *)
	Valid.is_empty candidates.cd_valid
	||
	(* or best forgotten element is better than best cached element *)
	compare_candidates ~different:true forgotten_barrier (Valid.min candidates.cd_valid) < 0
      then begin
	(* rebuild candidates for the forgotten element *)

	(* no candidate is forgotten anymore *)
	ForgottenBarrier.remove candidates.cd_forgotten_barrier forgotten_element;
	candidates.cd_forgotten_order <- ForgottenOrder.remove (forgotten_element, forgotten_barrier) candidates.cd_forgotten_order;


	(* ... and recompute them *)

	(* ... but candidates below the previous barier should be ignored upon recomputation,
	   so enter guard ... *)
	candidates.cd_forgotten_recomputation_barrier <- Some (forgotten_element, forgotten_barrier);

	Problem_literals.compute_for_element candidates.cd_problem_literals forgotten_element Context_unifier.Assert;
	ignore (build_unprocessed ~force:true candidates : bool);

	(* ... and exit guard *)
	candidates.cd_forgotten_recomputation_barrier <- None;

	rebuild_forgotten candidates;
      end
  end


let rec get_best_candidate (candidates: candidates) : (candidate * literal * int) option =
  (* assure that the best candidate is present *)
  rebuild_forgotten candidates;

  if Valid.is_empty candidates.cd_valid then begin
    None
  end

  else begin
    let candidate =
      Valid.remove_min candidates.cd_valid
    in
      (* eagerly drop retracted candidates, if any *)
      retract_valid_min candidates;

      (* candidate is no longer valid (due to backtracking) *)
      if Context_unifier.is_raw_context_unifier_invalid candidate.ca_raw_context_unifier then begin
	get_best_candidate candidates;
      end
	
      else begin
	(* contains doesn't work with compacted literals *)
	if false && Const.debug then begin
	  Array.iter
	    (fun context_partner ->
	       if not (Context.contains candidates.cd_context context_partner.Context_unifier.cp_element) then begin
		 print_endline (Context_unifier.raw_context_unifier_to_string candidate.ca_raw_context_unifier);
		 print_endline (Term.literal_to_string context_partner.Context_unifier.cp_element.Context.el_literal);
		 failwith "Selection_assert.context";
	       end
	    )
	    candidate.ca_raw_context_unifier.Context_unifier.rcu_context_partners
	end;
	let assert_literal, index =
	  build_assert_literal candidate.ca_raw_context_unifier
	in
	  match Context.check_subsumed candidates.cd_context assert_literal with
	    | Some _ ->
		(* candidate is not applicable with the context, so drop it *)
		get_best_candidate candidates;
		
	    | None ->
		(* applicable candidate found *)
		Some (candidate, assert_literal, index)
      end
  end



	      
let select (candidates: candidates) : selected option =
  (* build all unprocessed candidates *)
  ignore (build_unprocessed ~force:true candidates : bool);
  
  let candidate =
    match candidates.cd_preselected with
      | Some _ as conflicting ->
	  (* if there a is lookahead closing candidate, chose it *)
	  candidates.cd_preselected <- None;
	  conflicting
	  
      | None ->
	  get_best_candidate candidates
  in
    match candidate with
      | None ->
	  (* all candidates processed,
	     so all lookahead candidates must be subsumed by the context by now and can be dropped. *)
	  candidates.cd_lookahead <- Selection_lookahead.create Const.max_assert_lookahead;
	  None
	    
      | Some (candidate, assert_literal, index) ->
	  (* added candidates are just forgotten.
	     as they are added eagerly in the choice point of their creation
	     they are removed on backtracking anyway. *)

	  let assert_literal =
	    Term.insert_literal assert_literal
	  in
	  let context_literals =
	    Context_unifier.get_context_literals
	      candidate.ca_raw_context_unifier.Context_unifier.rcu_context_partners
	  in
	  let context_unifier =
	    Context_unifier.recompute_full_unifier ~recompute:true candidate.ca_raw_context_unifier
	  in
	    State.apply_propagation
	      candidates.cd_state
	      assert_literal
	      context_unifier
	      candidate.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_clause
	      (Context_unifier.get_constrained_clause candidate.ca_raw_context_unifier)
	      candidate.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_vars
	      index
	      (Context_unifier.create_space_utility_inc candidate.ca_raw_context_unifier.Context_unifier.rcu_space)
	      context_literals;
	    
	    Some {
	      Selection_types.candidate_type = State.Propagation;
	      Selection_types.literal = assert_literal;
	      Selection_types.raw_context_unifier = candidate.ca_raw_context_unifier;
	    }





(* check all exceeding candidates for applicability. *)
let check_exceeding (candidates: candidates) : unit =
  if Config.is_horn candidates.cd_config then begin
  if not (candidates.cd_unprocessed_size = 0) then
    failwith "Selection_assert.check_exceeding";

  (* enter exceeding checking mode *)
  candidates.cd_check_exceeding <- true;

  (* recompute for each context literal all candidates in chronological order *)
  begin
    try
      Context.iter
	(fun element ->
	   if is_marked_exceeding candidates element then
	     Problem_literals.compute_for_element candidates.cd_problem_literals element Context_unifier.Assert;
	)
	candidates.cd_context;

      (* build all unprocessed exceeding candidates *)
      ignore (build_unprocessed ~force:true candidates: bool);
    with
      | EXCEEDING_EXHAUSTED ->
	  ()
  end;

  (* unprocessed candidates might be in an inconsistent state due to EXCEEDING_EXHAUSTED.
     as it can only contain recomputed candidates, it can be savely emptied. *)
  clear_unprocessed candidates;

  (* leave exceeding checking mode *)
  candidates.cd_check_exceeding <- false

  end



(*** backtrack ***)

let backtrack_forgotten (candidates: candidates) : unit =
  (* if recomputation was done, it is finished by now. *)
  candidates.cd_forgotten_recomputation_barrier <- None;

  (* remove barriers of retracted context elements *)
  let invalid_elements =
    ForgottenBarrier.fold
      (fun element candidate acc ->
	 if State.is_choice_point_invalid element.Context.el_choice_point then
	   (element, candidate) :: acc

	 else
	   acc
      )
      candidates.cd_forgotten_barrier
      []
  in
    List.iter
      (fun (element, _) ->
	 ForgottenBarrier.remove candidates.cd_forgotten_barrier element;
      )
      invalid_elements;

    candidates.cd_forgotten_order <-
      List.fold_left
        (fun forgotten (element, candidate) ->
	   ForgottenOrder.remove (element, candidate) forgotten 
	)
        candidates.cd_forgotten_order
        invalid_elements



let backtrack_exceeding (candidates: candidates) : unit =
  (* remove candidates exceeding the depth bound. *)
  let invalid_elements =
    Hashtbl.fold
      (fun id element acc ->
	 if State.is_choice_point_invalid element.Context.el_choice_point then
	   id :: acc

	 else
	   acc
      )
      candidates.cd_exceeding_elements
      []
  in
    List.iter
      (fun id ->
	 Hashtbl.remove candidates.cd_exceeding_elements id
      )
      invalid_elements


  

(* removes candidates based on context unifiers which do not longer exist after backtracking,
   and also backtrackes the close lookahead. *)
let backtrack (candidates: candidates) : unit =
  (* remove closing lookahead candidate. *)
  begin
    match candidates.cd_preselected with
      | None -> ()
      | Some (candidate, _, _) ->
	 if Context_unifier.is_raw_context_unifier_invalid candidate.ca_raw_context_unifier then
	   candidates.cd_preselected <- None
	 else
	  failwith "Selection_assert.backrack: preselected"
  end;

  (* the lookahead within the bound is exhaustively processed before the next split or close,
     so it can just be dropped. *)
  candidates.cd_lookahead <- Selection_lookahead.create Const.max_assert_lookahead;
  (* the lookahead without the bound may still be useful. *)
  Selection_lookahead.backtrack candidates.cd_lookahead_exceeding;

  (* remove the easy to remove retracted valid candidates,
     remove the others later on when they are retracted from the queue. *)
  retract_valid_min candidates;
  retract_valid_max candidates;

  backtrack_forgotten candidates;
  backtrack_exceeding candidates;

  (* remove unprocessed candidates *)
  clear_unprocessed candidates
