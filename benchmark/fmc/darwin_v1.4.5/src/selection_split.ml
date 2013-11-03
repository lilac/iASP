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
type choice_point = State.choice_point
type literal = Term.literal
type pureness = Term_attributes.pureness
type subst = Subst.subst
type state = State.state
type context_unifier_space = Context_unifier.context_unifier_space
type raw_context_unifier = Context_unifier.raw_context_unifier
type problem_literals = Problem_literals.problem_literals
type selected = Selection_types.selected
type context = Context.context
type space_registry = Context_unifier_space.space_registry

(* an instance computed by a context unifier for a clause and a context literal.
   a remainder literal, if cl_is_remainder_literal is true *)
type candidate_literal = {
  (* the instance *)
  cl_literal: literal;

  (* the context literal used to compute the remainder literal *)
  cl_context_element: Context.element;

  (* is this a remainder literal? *)
  cl_is_remainder_literal: bool;

  (* the index of the context literal in the context unifier,
     based on the nth clause literal, starting from 0 *)
  cl_index: int;
}

(* attributes of a candidate for determining the 'best' candidate *)
type quality = {
  (* size of the remainder, not the clause
     using the clause size as an additional comparison attribute didn't pay off. *)
  cq_remainder_size: int;

  (* pureness of the candidate literal. *)
  cq_pureness: pureness;

  (* depth or weight, depending on the restart bound method. *)
  cq_term_quality: int;

  (* see Context_unifier.generation_of_candidate *)
  cq_generation: int;

  cq_sort : Symbol.sort;
}

(* a split candidate *)
type candidate = {
  (* the computed context unifier *)
  ca_raw_context_unifier: raw_context_unifier;

  (* comparison properties for the selection heuristic *)
  ca_quality: quality;
}

(* a right split, i.e. an applied and retracted left split candidate *)
type right_split = {
  (* the split candidate *)
  rs_candidate: candidate;

  (* the corresponding _left_ split literal *)
  rs_literal: literal;

  (* the index of the context literal in the context unifier,
     based on the nth clause literal, starting from 0 *)
  rs_index: int;

  (* the explanation of the right split,
     i.e. conflict set for dependency directed backtracking. *)
  rs_explanation: literal array;
}



(*** comparison ***)

(* :TODO: add clause utility *)
let compare_candidates (first: candidate) (second: candidate) : int =
  let con =
    match first.ca_quality.cq_sort, second.ca_quality.cq_sort with
      | Symbol.Connection, Symbol.Connection -> 0
      | Symbol.Connection, _ -> 1
      | _, Symbol.Connection -> -1
      | _ -> 0
  in
    if con <> 0 then
      con
    else

  (* universality *)
  let pureness_universal =
    Term_attributes.cmp_pureness first.ca_quality.cq_pureness second.ca_quality.cq_pureness
  in
    if pureness_universal <> 0 then
      pureness_universal
    else begin
      (* remainder size *)
      if first.ca_quality.cq_remainder_size < second.ca_quality.cq_remainder_size then
	-1
      else if first.ca_quality.cq_remainder_size > second.ca_quality.cq_remainder_size then
	1
      else begin
	(* remainder literal term quality *)
 	if first.ca_quality.cq_term_quality < second.ca_quality.cq_term_quality then
	  -1
	else if first.ca_quality.cq_term_quality > second.ca_quality.cq_term_quality then
	  1
	else begin
	  (* number of parameters *)
	  let pureness_generalization =
	    Term_attributes.cmp_universality first.ca_quality.cq_pureness second.ca_quality.cq_pureness
	  in
	    if pureness_generalization <> 0 then
	      pureness_generalization
	    else begin
	      (* generation *)
	      if first.ca_quality.cq_generation < second.ca_quality.cq_generation then
		-1
	      else if first.ca_quality.cq_generation > second.ca_quality.cq_generation then
		1
	      else
		
		(*(* clause utility *)
		let cmp =
		  Tools.compare_int
		     !(second.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_utility)
		     !(first.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_utility)
		in
		  if cmp <> 0 then
		    cmp
		  else
		*)
		(* arbitrary total ordering *)
		Context_unifier.compare_context_unifiers ~different:Const.stable_derivation
		  first.ca_raw_context_unifier second.ca_raw_context_unifier
	    end
	end
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


(* for potentially applicable candidates *)	    
module Valid =
  Heap.MinMaxHeap (
    struct
      type t = candidate

      let compare = compare_candidates
      let to_string = candidate_to_string ""
    end
  )


(* remainders whose best split literal are of this depth *)
type candidates = {
  (* environment *)
  cd_config: config;
  cd_bound: bound;
  cd_statistic: statistic;
  cd_state: state;
  cd_context: context;
  cd_problem_literals: problem_literals;
  cd_space_registry: space_registry;

  (* candidates not yet considered *)
  cd_unprocessed: raw_context_unifier array;
  (* number of valid entries *)
  mutable cd_unprocessed_size: int;

  (* the remainders currently valid for adding to the context
     (at least valid when last checked). *)
  cd_valid: Valid.t;

  (* candidates which are not applicable because of a context literal
     added to the context at the stored choice point. *)
  mutable cd_invalid: (raw_context_unifier * choice_point) list;

  (* candidates added to the context with:
     - split_literal
     - index of split_literal within context unifier
     - choice point created by splitting on it
  *)
  mutable cd_added: (candidate * literal * int * choice_point) list;

  (* currently implied but not yet applied right splits.

     there can actually be more than only one of these,
     as backtracking does not necessarily backjump to the implication choice point
     of the currently computed right split,
     but may jump shorter (see implementation State_backtrack.backtrack).

     in order to be able to replay guiding paths (see module Jumping)
     the right splits must be redone in the exact same order
     of the corresponding left splits.
     therefore:
     - cd_right_splits is in reverse chronological order of corresponding left splits,
       i.e. the head is the most recently rectracted split
       (except for applied right splits)

     - cd_applied_right_splits are the applied right splits in reversed order,
       i.e. the head is the most recently applied right split.

     thus, (List.rev cd_applied_right_splits) @ cd_right_splits
     gives applied and not applied right splits in
     reverse chronological application order of the corresponding left splits.
  *)
  mutable cd_right_splits: (candidate * literal * int * literal array) list;

  (* currently implied and applied right splits, with the application choice point *)
  mutable cd_applied_right_splits: ((candidate * literal * int * literal array) * choice_point) list;


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
}


(* raised if the current checking of exceeding candidates
   already implies a restart with the next higher deepening bound.
   that is, further evaluation of exceeding candidates
   yields no further information and can be omitted. *)
exception EXCEEDING_EXHAUSTED










    
(*** creation ***)

let null_quality = {
  cq_remainder_size = -1;
  cq_pureness = Term_attributes.get_pureness Term.null_literal.Term.atom;
  cq_term_quality = -1;
  cq_generation = -1;
  cq_sort = Symbol.Skolem;
}

let null_candidate = {
  ca_raw_context_unifier = Context_unifier.null_context_unifier;
  ca_quality = null_quality;
}



let create (config: config) (bound: bound) (statistic: statistic) (state: state)
  (context: context) (problem_literals: problem_literals) (space_registry: space_registry) : candidates =

  (* min. 1 entry necessary for function add *)
  let unprocessed =
    (* no split candidates at all for horn, so no need for array creation  *)
    if Config.is_horn config then
      [| |]
    else
      Array.make (max 1 Const.max_unprocessed_split_candidates) Context_unifier.null_context_unifier
  in
  {
    cd_config = config;
    cd_bound = bound;
    cd_statistic = statistic;
    cd_state = state;
    cd_context = context;
    cd_problem_literals = problem_literals;
    cd_space_registry = space_registry;

    cd_unprocessed = unprocessed;
    cd_unprocessed_size = 0;
    cd_valid = Valid.create null_candidate;
    cd_invalid = [];
    cd_added = [];
    cd_right_splits = [];
    cd_applied_right_splits = [];

    cd_exceeding_elements = Hashtbl.create 64;
    cd_check_exceeding = false;
  }




(*** misc ***)


let size (candidates: candidates) : int =
  Valid.size candidates.cd_valid




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


let get_remainder_size (candidate_literals: candidate_literal list) : int =
  List.fold_left
    (fun acc remainder_literal ->
       if remainder_literal.cl_is_remainder_literal then
	 acc + 1
       else
	 acc
    )
    0
    candidate_literals


(* denotes that a check of a candidate against the context failed
   because of this context element.
   used to early abort the building of a remainder.
*)
exception CONFLICT of Context.element


(* builds the candidate literals generated by a context unifier.

   always returns the list in the same order for a context unifier
   in order to make the derivation more stable to (conceptually) independent changes,
   e.g. when find_best_candidate_literal picks the first of the remainder literals
   equal to the heuristics.

   if ~check is true,
   the candidate literals are checked for applicability while being built:
   - remainder literal: p-subsumption, conflict
   - all candidate literals: productivity

   iff the candidate is not applicable CONFLICT is raised.

   insert_db: see module Term.
*)
let build_candidate_literals ?(insert_db: bool = false) ~(check: bool)
    (config: config) (context: context) (raw_context_unifier: raw_context_unifier)
    : candidate_literal list =
(*  print_endline ("\nBUILD :");
  print_endline (Context_unifier.raw_context_unifier_to_string raw_context_unifier);
*)
  let subst =
    Context_unifier.recompute_unifier ~recompute:true raw_context_unifier
  in
  let remainder_states =
    Context_unifier.compute_remainder_states raw_context_unifier subst
  in
  let admissible =
    Admissible.make_admissible config raw_context_unifier subst remainder_states
  in

  if Const.debug then begin
    let remainder_choice_points_admissible =
      Context_unifier.compute_remainder_states raw_context_unifier subst
    in
      if not (remainder_states = remainder_choice_points_admissible) then begin
	failwith "Selection_split.remainder_choice_points";
      end;
  end;
    
  let input_partners =
    raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_input_partners
  in

  (* input partners might not be in the right order,
     so build the remainder literals out of order, store them in this ordered array,
     and then convert them to an ordered list *)
  let candidate_literals =
    Array.make
      (Array.length input_partners)
      {
	cl_context_element = Context.null_element;
	cl_literal = Term.null_literal;
	cl_is_remainder_literal = false;
	cl_index = -1;
      }
  in
    Array.iter
      (fun input_partner ->
	 let index =
	   input_partner.Context_unifier.ip_index
	 in
	 let context_partner =
	   raw_context_unifier.Context_unifier.rcu_context_partners.(index)
	 in
	 let context_element =
	   context_partner.Context_unifier.cp_element;
	 in
	 let split_literal =
	   Subst.apply_to_literal ~insert_db:insert_db
	     admissible
	     input_partner.Context_unifier.ip_literal
	     Subst.input_literal_offset
	 in
	   (* remainder literals contradictory with skolem context literals
	      are to be ignored as remainder literals.
	      that is, as the corresponding context unifier against that context literal
	      is not computed, so we have to reuse the original context unifier,
	      but pick a different remainder literal this time. *)
	   if Const.ignore_skolem_literals
	     &&
	     Context.check_skolem_contradictory context split_literal then begin
	       remainder_states.(index) <- false;
	   end;

	   (* if this literal is not applicable abort the computation. *)
	   if check then begin
	     (* do cheap checks first *)
	     if remainder_states.(index) then begin
	       begin
		 match Context.check_subsumed context split_literal with
		   | None -> ()
		   | Some conflict ->
		       raise (CONFLICT conflict)
	       end;

	       begin
		 match Context.check_contradictory context split_literal with
		   | None -> ()
		   | Some conflict -> 
		       raise (CONFLICT conflict)
	       end;
	       
	     end;

	     (* only check remainder literals for productivity -
	        non-remainder literals are almost always productive for mixed literals,
		and always productive for pure literals. *)
	     if remainder_states.(index) && Config.productivity config then begin
	       (* Potential optimization:
		  a resolving element is always productive,
		  it's whole purpose is to invalidate all context unifiers
		  build on the larger clause.
		  would need a flag showing that context_partner is resolved
	       *)
		 match
		   Context.check_productive context context_element.Context.el_literal split_literal
		 with
		   | None -> ()			 
		   | Some conflict ->
		       if Const.debug && Const.ignore_skolem_literals && conflict.Context.el_skolemized then
			 failwith ("NOT PRODUCTIVE BECAUSE OF SKOLEM LITERAL: " ^ Term.literal_to_string conflict.Context.el_literal);
		       raise (CONFLICT conflict)
	     end;
	   end;
	   
	   (* applicable, store *)
	   let candidate_literal = {
	     cl_context_element = context_element;
	     cl_literal = split_literal;
	     cl_is_remainder_literal = remainder_states.(index);
	     cl_index = index;
	   }
	   in
	     candidate_literals.(index) <- candidate_literal
      )
      input_partners;

    Array.to_list candidate_literals




let remainder_literals_to_string_ (candidate_literals: candidate_literal list) : string =
  let candidate_literals_repr =
    List.fold_left
      (fun acc candidate_literal ->
	 if candidate_literal.cl_is_remainder_literal then
	   Term.literal_to_string candidate_literal.cl_literal :: acc
	 else
	   "___" :: acc
      )
      []
      candidate_literals
  in
    "Remain.: " ^ "[" ^ String.concat ", " (List.rev candidate_literals_repr) ^ "]" ^ "\n"



(* find the 'best' remainder literal of the candidate
   returns it, plut its pureness, term_quality, and complexity *)
let find_best_candidate_literal (candidates: candidates) (candidate_literals: candidate_literal list) :
  (candidate_literal * pureness * int * Bound.complexity) =
  let best =
    List.fold_left
      (fun acc candidate_literal ->
	 (* is this a remainder literal? *)
	 if not candidate_literal.cl_is_remainder_literal then begin
	   acc
	 end

	 (* is this literal contradictory with a skolem context literal? *)
	 else if Const.ignore_skolem_literals &&
	   Context.check_skolem_contradictory candidates.cd_context candidate_literal.cl_literal
	 then begin
	   acc
	 end

	 else begin
	   let pureness = Term_attributes.get_pureness candidate_literal.cl_literal.Term.atom
	   and term_quality = get_term_quality candidates candidate_literal.cl_literal
	   and complexity = candidates.cd_bound#get_complexity candidate_literal.cl_literal
	   in
	   let cmp =
	     match acc with
	       | None ->
		   1
		     
	       | Some (_best_candidate_literal, best_pureness, best_term_quality, best_complexity) ->
		   (* if possible ignore candidates exceeding the depth bound *)
		   if
		     candidates.cd_bound#exceeds best_complexity
		     &&
		     candidates.cd_bound#exceeds complexity
		   then
		     0
		       
		   else
		     (* candidate must be within the complexity bound *)
		     let cmp_complexity =
		       if candidates.cd_bound#exceeds complexity then
			 -1
		       else if candidates.cd_bound#exceeds best_complexity then
			 1
		       else
			 0
		     in
		       if cmp_complexity <> 0 then
			 cmp_complexity
		       else
			 (* per clause:
			    positive connection is worst,
			    negative connection is best

			    per candidate:
			    connection is worst.
			    so each clause has to get rid of all negative connections
			    first before the rest can be processed. *)
			 let cmp_con =
			   let sort = Term.get_literal_sort candidate_literal.cl_literal in
			   let sign = candidate_literal.cl_literal.Term.sign in
			   let sort_best = Term.get_literal_sort _best_candidate_literal.cl_literal in
			   let sign_best = _best_candidate_literal.cl_literal.Term.sign in
			     match sort, sort_best with
			       | Symbol.Connection, Symbol.Connection ->
				   if sign = sign_best then 0
				   else if sign then 1
				   else -1
			       | Symbol.Connection, _ ->
				   if sign then 1 else -1
			       | _, Symbol.Connection ->
				   if sign_best then -1 else 1
			       | _ -> 0
			 in
			   if cmp_con <> 0 then
			     cmp_con
			   else

			 let cmp_pureness =
			   Term_attributes.cmp_pureness best_pureness pureness
			 in
			   if cmp_pureness <> 0 then
			     cmp_pureness
			   else
			     
			     let cmp_term_quality =
			       compare best_term_quality term_quality
			     in
			       if cmp_term_quality <> 0 then
				 cmp_term_quality
			       else
				 Term_attributes.cmp_universality best_pureness pureness
	   in
	     if cmp <= 0 then
	       acc
	     else
	       Some (candidate_literal, pureness, term_quality, complexity)
	 end
      )
      None
      candidate_literals
  in
    match best with
      | None ->
	  print_endline (remainder_literals_to_string_ candidate_literals);
	  failwith "Selection_split.find_best_candidate_literal"
	    
      | Some best ->
	  best




(*** representation ***)



let remainder_literals_to_string (config: config) (context: context)
  (raw_context_unifier: raw_context_unifier) : string =

  let remainder_literals =
    build_candidate_literals ~check:false config context raw_context_unifier
  in
    remainder_literals_to_string_ remainder_literals




let remainder_to_string_ (candidates: candidates) (title: string)
  (remainder: candidate) (best_remainder_literal: literal) (remainder_literals: candidate_literal list) : string =
    State.active_choice_point_to_string candidates.cd_state
    ^ title ^ ": " ^ Term.literal_to_string best_remainder_literal ^ "\n"
    ^
    if Config.print_derivation_context_unifier candidates.cd_config then
      Context_unifier.raw_context_unifier_to_string remainder.ca_raw_context_unifier
      ^ remainder_literals_to_string_ remainder_literals
    else
      ""




let remainder_to_string (candidates: candidates) (title: string) (remainder: candidate) : string =
  let remainder_literals =
    build_candidate_literals ~check:false
      candidates.cd_config candidates.cd_context remainder.ca_raw_context_unifier
  in
  let (best_candidate_literal, _, _, _) =
    find_best_candidate_literal candidates remainder_literals
  in
    remainder_to_string_ candidates title remainder best_candidate_literal.cl_literal remainder_literals









(*** add ***)



(* add a candidate to the valid candidate queue. *)
let add_valid (candidates: candidates) (candidate: candidate) : unit =
  try
    Valid.add candidates.cd_valid candidate
  with
    | Heap.OVERFLOW ->
	raise (Const.NO_SOLUTION "Selection_split.add_valid: candidate set overflow")



(* build a candidate data structure *)
let build_candidate' (raw_context_unifier: raw_context_unifier)
    (remainder_size: int) (pureness: pureness) (term_quality: int) (sort : Symbol.sort)
    : candidate =

  let quality = {
    cq_remainder_size = remainder_size;
    cq_pureness = pureness;
    cq_term_quality = term_quality;
    cq_generation = Context_unifier.generation_of_candidate raw_context_unifier.Context_unifier.rcu_context_partners;
    cq_sort = sort;
  }
  in
    {
      ca_raw_context_unifier = raw_context_unifier;
      ca_quality = quality;
    }


let build_candidate (candidates: candidates) (raw_context_unifier: raw_context_unifier) : unit =
(*  
  print_endline ("\nBUILD :");
  print_endline (Context_unifier.raw_context_unifier_to_string raw_context_unifier);
*)
  (* don't build or check exceeding candidates for an element
     that has to be rebuild anyway *)
  try
    let candidate_literals =
      build_candidate_literals ~insert_db:false ~check:true
	(* checking for satisfied candidates didn't do any good.
	  ~check_true:candidates.cd_check_exceeding *)
	candidates.cd_config candidates.cd_context raw_context_unifier
    in
    let best_candidate_literal, pureness, term_quality, complexity =
      find_best_candidate_literal candidates candidate_literals
    in      
    let split_literal =
      best_candidate_literal.cl_literal;
    in
    let remainder_size =
      get_remainder_size candidate_literals
    in

      (* are we checking exceeding candidates for applicability? *)
      if candidates.cd_check_exceeding then begin
	ignore (
	  candidates.cd_bound#register
	    complexity
	    (Context_unifier.get_creation_choice_point raw_context_unifier.Context_unifier.rcu_context_partners)
	    : bool);

	if candidates.cd_bound#has_min_exceeding then
	  raise EXCEEDING_EXHAUSTED;
      end

      (* exceeding *)
      else if candidates.cd_bound#exceeds complexity then begin
	(* keep candidates? *)
	match Config.restart candidates.cd_config with
	  | Flags.RS_Eager
	  | Flags.RS_Lazy ->
	      (* all remainder literals within the depth bound are contradictory
		 with skolem literals, but some exceeding literals are not. *)
	      if Const.ignore_skolem_literals then begin
		(* move to invalid candidates *)
		candidates.cd_invalid <-
  		  (raw_context_unifier, State.active_choice_point candidates.cd_state)
		:: candidates.cd_invalid
	      end

	      else begin
		print_endline (Context_unifier.raw_context_unifier_to_string raw_context_unifier);
		failwith "Selection_split.build_candidate: exceeding candidate without delayed restart."
	      end
	      
	  | Flags.RS_Delayed ->
	      let context_element =
		Context_unifier.creating_context_element raw_context_unifier
	      in
		(* we don't consider initial candidates based only on -v to be exceeding.
		   it's too complicate to recompute them,
		   so they are just kept and treated as if not exceeding. *)
		if
		  Context.element_equal Context.plus_v_element context_element
		  ||
		  Context.element_equal Context.minus_v_element context_element
		then begin
		  let candidate =
		    build_candidate' raw_context_unifier remainder_size pureness term_quality
		      (Term.get_literal_sort split_literal)
		  in
		    (* an applicable candidate has been found, keep it. *)
		    if Config.print_split_candidates candidates.cd_config then begin
			print_string (remainder_to_string_
				       candidates
				       "New Split Candidate"
				       candidate
				       split_literal
				       candidate_literals);
		      end;
		    
		    add_valid candidates candidate;
		  end

		(* for this context literal exceeding applicable candidates
		   have already been dropped. *)
		else if is_marked_exceeding candidates context_element then
		  ()
		    
		else
		  mark_exceeding candidates context_element;
      end

      (* regular candidates within the deepening bound. *)	
      else begin
	let candidate =
	  build_candidate' raw_context_unifier remainder_size pureness term_quality
	    (Term.get_literal_sort split_literal)
	in
	  (* an applicable candidate has been found, keep it. *)
	  if Config.print_split_candidates candidates.cd_config then begin
	    print_string (remainder_to_string_
			    candidates
			    "New Split Candidate"
			    candidate
			    split_literal
			    candidate_literals);
	  end;

	  add_valid candidates candidate;
      end 
  with
    | CONFLICT conflicting_element ->
	(* candidate is not applicable *)

	(* drop on recomputation *)
	if candidates.cd_check_exceeding then
	  ()

	(* permanently not applicable, so drop it. *)	  
	else if 
	  Context_unifier.is_permanently_blocking
	    conflicting_element
	    raw_context_unifier.Context_unifier.rcu_context_partners
	then
	  ()
	    
	(* register exceeding candidates for the Delayed restarting policy.
	   (there can be no exceeding candidates otherwise)
	   actually, right here the candidate is invalid and no problem
	   for the Delayed strategy.
	   But, if on backtracking this candidate is not invalid anymore,
	   the context element has to be marked.
	   
	   So,
	   a)we keep all invalid exceeding candidates,
	     and mark the context element only if they become valid on backtracking
	     Problem: memory consumption
	   b)we eagerly mark the context element as done here,
	     and just drop the element
	   c)or, we store exceeding candidates for not yet marked elements,
	     and only mark the element if the candidate becomes valid on backtracking.

	   this implementation follows c).
	*)
	else if
	  Config.restart candidates.cd_config = Flags.RS_Delayed
	  &&
	  raw_context_unifier.Context_unifier.rcu_exceeding_complexity
	then begin
	  let context_element =
	    Context_unifier.creating_context_element raw_context_unifier
	  in
	    (* don't care, will be recomputed anyway *)
	    if is_marked_exceeding candidates context_element then
	      ()		    
	    else
	      candidates.cd_invalid <-
 		(raw_context_unifier, conflicting_element.Context.el_choice_point) :: candidates.cd_invalid
	end
	  
	(* move it to the invalid candidates. *)
	else
	  candidates.cd_invalid <-
  	    (raw_context_unifier, conflicting_element.Context.el_choice_point) :: candidates.cd_invalid
	


(*** unprocessed ***)

let clear_unprocessed (candidates: candidates) : unit =
  for index = 0 to candidates.cd_unprocessed_size - 1 do
    candidates.cd_unprocessed.(index) <- Context_unifier.null_context_unifier;
  done;
  candidates.cd_unprocessed_size <- 0


(* only processes the new candidates and empties the buffer
   if the buffer is full or force is true. *)
let build_unprocessed ~(force: bool) (candidates: candidates) : unit =
  (* buffer not full, do nothing *)
  if
    not force
    &&
    candidates.cd_unprocessed_size < Array.length candidates.cd_unprocessed - 1
  then begin
    ()
  end

  (* build and check all new candidates *)
  else begin
    for index = 0 to candidates.cd_unprocessed_size - 1 do
      (* ignore unprocessed candidates invalidated by backtracking *)
      if not (Context_unifier.is_raw_context_unifier_invalid candidates.cd_unprocessed.(index)) then begin
	build_candidate candidates candidates.cd_unprocessed.(index);
      end;

      candidates.cd_unprocessed.(index) <- Context_unifier.null_context_unifier;
    done;
    candidates.cd_unprocessed_size <- 0
  end


let add_unprocessed (candidates: candidates) (candidate: raw_context_unifier) : unit =
  (* store the new candidate for later processing *)
  candidates.cd_unprocessed.(candidates.cd_unprocessed_size) <- candidate;
  candidates.cd_unprocessed_size <- candidates.cd_unprocessed_size + 1;

  build_unprocessed ~force:false candidates

let add (candidates: candidates) (candidate: raw_context_unifier) : unit =
  Statistic.inc_computed_split_candidates candidates.cd_statistic;
  add_unprocessed candidates candidate








(*** select ***)

(* get the best of all valid candidates, i.e. the first still valid one. *)
let rec get_best_candidate (candidates: candidates) : (candidate * literal * int) option =
  if Valid.is_empty candidates.cd_valid then
    None

  else begin
    let candidate =
      Valid.remove_min candidates.cd_valid
    in
      (* candidate does no longer exist (due to backtracking) *)
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
		 failwith "Selection_split.context";
	       end
	    )
	    candidate.ca_raw_context_unifier.Context_unifier.rcu_context_partners
	end;
	try
	  let candidate_literals =
	    build_candidate_literals ~check:true
	      candidates.cd_config candidates.cd_context candidate.ca_raw_context_unifier
	  in
	  let (best_candidate_literal, _, _, _) =
	    find_best_candidate_literal candidates candidate_literals
	  in
	    (* applicable candidate found *)
	    Some (candidate, best_candidate_literal.cl_literal, best_candidate_literal.cl_index)
	      
	with
	  | CONFLICT conflicting_element ->
	      (* candidate is not applicable *)
	      begin
		if
		  Context_unifier.is_permanently_blocking
		  conflicting_element
		    candidate.ca_raw_context_unifier.Context_unifier.rcu_context_partners
		then
		  ()
		    
		else
		  candidates.cd_invalid <-
		    (candidate.ca_raw_context_unifier, conflicting_element.Context.el_choice_point) :: candidates.cd_invalid
	      end;
	      
	      get_best_candidate candidates;
      end
  end


(* selected split literal matches the guiding branch? *)
let check_replay_literal (candidate: candidate) (saved_literal: literal) (split_literal: literal) : unit =
  if
    Const.debug
    &&
      not (Term.are_literals_skolem_variants saved_literal split_literal)
  then begin
    print_endline ("SAVED: " ^ Term.literal_to_string saved_literal);
    print_endline ("FOUND: " ^ Term.literal_to_string split_literal);
    print_endline (Context_unifier.raw_context_unifier_to_string candidate.ca_raw_context_unifier);
    failwith "Selection_split.check_replay_literal";
  end


(* do the split on the given candidate.
   if replay is given a guiding branch is replayed
   and the corresponding candidate must match and is used as specified.
*)
let split (candidates: candidates)
   (candidate: candidate) (split_literal: literal) (index: int)
   (replay: Jumping.guiding_step option) : selected =

  let split_literal =
    Term.insert_literal split_literal
  in
  let context_unifier =
    Context_unifier.recompute_full_unifier ~recompute:true candidate.ca_raw_context_unifier
  in

  (* unit split? *)
  if candidate.ca_quality.cq_remainder_size = 1 then begin
    let context_literals =
      Context_unifier.get_context_literals
	candidate.ca_raw_context_unifier.Context_unifier.rcu_context_partners
    in
      (* do the split *)
      State.apply_propagation
	candidates.cd_state
	split_literal
	context_unifier
	candidate.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_clause
	(Context_unifier.get_constrained_clause candidate.ca_raw_context_unifier)
	candidate.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_vars
	index
	(Context_unifier.create_space_utility_inc candidate.ca_raw_context_unifier.Context_unifier.rcu_space)
	context_literals;

      (* save candidate for later use *)
      candidates.cd_invalid <-
	(candidate.ca_raw_context_unifier, State.active_choice_point candidates.cd_state) :: candidates.cd_invalid;

      {
	Selection_types.candidate_type = State.Propagation;
	Selection_types.literal = split_literal;
	Selection_types.raw_context_unifier = candidate.ca_raw_context_unifier;
      }
  end
    
  else begin
    match replay with
      | None ->
	  (* ordinary left split *)
	  let split_choice_point: choice_point =
	    State.apply_split_left
	      candidates.cd_state
	      split_literal
	      context_unifier
	      candidate.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_clause
	      (Context_unifier.get_constrained_clause candidate.ca_raw_context_unifier)
	      candidate.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_vars
	      index
	      (Context_unifier.create_space_utility_inc candidate.ca_raw_context_unifier.Context_unifier.rcu_space);
	  in
	    candidates.cd_added <-
	      (candidate, split_literal, index, split_choice_point) :: candidates.cd_added;
	
	    {
	      Selection_types.candidate_type = (*Selection_types*)State.SplitLeft;
	      Selection_types.literal = split_literal;
	      Selection_types.raw_context_unifier = candidate.ca_raw_context_unifier;
	    }


      | Some (Jumping.Split saved_literal) ->
	  (* replay guiding branch - full split *)
	  check_replay_literal candidate saved_literal split_literal;
	  
	  let split_choice_point: choice_point =
	    State.apply_split_left
	      candidates.cd_state
	      split_literal
	      context_unifier
	      candidate.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_clause
	      (Context_unifier.get_constrained_clause candidate.ca_raw_context_unifier)
	      candidate.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_vars
	      index
	      (Context_unifier.create_space_utility_inc candidate.ca_raw_context_unifier.Context_unifier.rcu_space);
	  in
	    candidates.cd_added <-
	      (candidate, split_literal, index, split_choice_point) :: candidates.cd_added;
	    
	    {
	      Selection_types.candidate_type = (*Selection_types*)State.SplitLeft;
	      Selection_types.literal = split_literal;
	      Selection_types.raw_context_unifier = candidate.ca_raw_context_unifier;
	    }

	| Some (Jumping.Left saved_literal) ->
	    (* replay guiding branch - assert left split *)
	    check_replay_literal candidate saved_literal split_literal;

	    State.apply_split_right
	      candidates.cd_state
	      split_literal
	      context_unifier
	      candidate.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_clause
	      (Context_unifier.get_constrained_clause candidate.ca_raw_context_unifier)
	      candidate.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_vars
	      index
	      (Context_unifier.create_space_utility_inc candidate.ca_raw_context_unifier.Context_unifier.rcu_space)
	      [| |];
		      
	    candidates.cd_invalid <-
	      (candidate.ca_raw_context_unifier, State.active_choice_point candidates.cd_state) :: candidates.cd_invalid;

	    {
	      (* :TODO: name different than right split *)
	      Selection_types.candidate_type = (*Selection_types*)State.SplitRight;
	      Selection_types.literal = split_literal;
	      Selection_types.raw_context_unifier = candidate.ca_raw_context_unifier;
	    }


	| Some (Jumping.Right saved_literal) ->
	    (* replay guiding branch - assert right split *)
	    let right_split_literal =
	      if Const.fd_right_split_parametric && Config.finite_domain candidates.cd_config then
		Term.request_negated_literal (Term.request_pure_literal ~universal:false split_literal)
	      else
		Term.request_negated_literal (Term.request_skolemized_literal split_literal)
	    in
	      check_replay_literal candidate saved_literal right_split_literal;

	      State.apply_split_right
		candidates.cd_state
		right_split_literal
		context_unifier
		candidate.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_clause
		(Context_unifier.get_constrained_clause candidate.ca_raw_context_unifier)
		candidate.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_vars
		index
		(Context_unifier.create_space_utility_inc candidate.ca_raw_context_unifier.Context_unifier.rcu_space)
		[| |];
	      
	      candidates.cd_invalid <-
		(candidate.ca_raw_context_unifier, State.active_choice_point candidates.cd_state) :: candidates.cd_invalid;
	      {
		Selection_types.candidate_type = (*Selection_types*)State.SplitRight;
		Selection_types.literal = right_split_literal;
		Selection_types.raw_context_unifier = candidate.ca_raw_context_unifier;
	      }
  end



(* is the right split conflicting with the current context?
   to early lemma unit propagation? *)
let check_right_split_consistent (candidates: candidates) (split_literal: literal) : unit =
  if Const.debug then begin
    let contradictory_element =
      Context.check_contradictory candidates.cd_context split_literal
    in
      match contradictory_element with
	| Some _ ->
	    failwith "Selection_split.check_right_split_consistent"
	      
	| None ->
	    ()
  end


(* select one of the current valid but not applied right splits. *)
let rec select_right_split (candidates: candidates) : selected option =
  
  match candidates.cd_right_splits with
    | [] ->
	None
	    
    | ((candidate, split_literal, index, context_literals) as right_split) :: tail ->
	(* remove the right split *)
	candidates.cd_right_splits <- tail;
	
	(* drop invalidated right splits *)
	if
	  Tools.array_exists
	    (fun literal -> Context.element_for_literal candidates.cd_context literal = None)
	    context_literals
	then begin
	  select_right_split candidates
	end

	(* apply this right split *)
	else begin
	  (* ignore if not necessary, i.e. effect already achieved by lemma propagation. *)
	  match Context.check_contradictory candidates.cd_context split_literal with
	    | Some _ ->
		select_right_split candidates
	      
	    | None ->
	  (* keep the right split for later? *)
	       begin
	    if Array.length context_literals = 0 then
	      (* incomplete branch - arbitrary backtracking,
		 no actual right split learned.
		 apply this right split once for completeness and then forget about it. *)
	      ()
		    
	    else
	      (* move the right split to the applied right splits *)
	      candidates.cd_applied_right_splits <-
		(right_split, State.active_choice_point candidates.cd_state) :: candidates.cd_applied_right_splits;
	  end;

	  (* we always do the right split.
	     conceptually, its effect could have already been carried out
	     some other assert on a lemma.
	     but we enforce doing right splits before lemma propagation.
	     the reason is that it is possible to ignore negative assert candidates.
	     but then, if lemma propagation is done first,
	     the right split could be inconsistent with the context
	     when it is finally selected.
	     for simplifying the resulting conflict analysis (or avoiding a new special case)
	     enforcing eager right splits is the easiest way. *)
	  let right_split =
	    (* can't add skolem constants to finite domains, so add parameter instead *)
	    if Const.fd_right_split_parametric && Config.finite_domain candidates.cd_config then
	      Term.request_negated_literal (Term.request_pure_literal ~universal:false split_literal)
	    else
	      Term.request_negated_literal (Term.request_skolemized_literal split_literal)
	  in
	    (*check_right_split_consistent candidates right_split;*)

      	    match Context.check_contradictory candidates.cd_context right_split with
	      | Some element ->
		  let input_partner =
		    Context_unifier.create_input_partner
		      0
		      right_split
		      [] (* don't care about variables *)
		  in
		  let universal =
		    Term.request_pure_literal ~universal:true right_split
		  in
		  let vars =
		    List.map
		      (fun var ->
			 Subst.make_var var Subst.input_literal_offset
		      )
		      (Term.vars_of_literal universal)
		  in
		    (* may not be a constraint *)
		    if Const.debug && Const.fd_constraint_solver
		      && Config.finite_domain candidates.cd_config &&
		      Term.is_fd_constraint universal
		    then
		      failwith ("Selection_split.select_right_split: right split is constraint: "
				^ Term.literal_to_string universal);

		  let space =
		    Context_unifier.create_space
		      (Context_unifier_space.get_id candidates.cd_space_registry)
		      (* need to be universal, otherwise learning generalized lemmas
			 will compute parameterized lemmas as well. *)
		      [universal]
		      vars
		      vars
		      vars
		      [| input_partner |]
		      [| |] (* don't care about ordering *)
		      (fun _ -> ()) (* no close candidates computed *)
		      (fun _ -> false) (* no assert candidates computed *)
		      (fun _ -> ()) (* no split candidates computed *)
		      (fun _ -> false) (* not used in recomputation of candidates *)
		      (fun _ -> false) (* not used in recomputation of candidates *)
		      true          (* this is said to be a lemma *)
		      None
		  in
		  let context_partner =
		    Context_unifier.create_context_partner
		      element
		      None
		      true
		  in
		  let context_unifier =
		    Context_unifier.create_context_unifier
		      space
		      [| context_partner |]
		      false
		      None (* we checked above that this will not be a constraint *)
		  in
		    raise (Context_unifier.CLOSE context_unifier)
		   
		  

	      | None ->	    
	  let context_unifier =
	    Context_unifier.recompute_full_unifier ~recompute:true candidate.ca_raw_context_unifier
	  in
	    State.apply_split_right
	      candidates.cd_state
	      right_split
	      context_unifier
	      candidate.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_clause
	      (Context_unifier.get_constrained_clause candidate.ca_raw_context_unifier)
	      candidate.ca_raw_context_unifier.Context_unifier.rcu_space.Context_unifier.cus_vars
	      index
	      (Context_unifier.create_space_utility_inc candidate.ca_raw_context_unifier.Context_unifier.rcu_space)
	      context_literals;
		      
	    Some
	      {
		Selection_types.candidate_type = (*Selection_types*)State.SplitRight;
		Selection_types.literal = right_split;
		Selection_types.raw_context_unifier = candidate.ca_raw_context_unifier;
	      }
	end
  

let select (candidates: candidates) (replay: Jumping.guiding_step option) : selected option =
  (* try right splits first *)
  match select_right_split candidates with
    | (Some _) as right_split ->
	right_split
	  
    | None ->
	(* the build all unprocessed candidates
	   and find the best left split *)
	build_unprocessed ~force:true candidates;

	let left_split =
	  get_best_candidate candidates
	in	    
	  match left_split with
	    | None ->
		None
		  
	    | Some (candidate, split_literal, index) ->
		Some (split candidates candidate split_literal index replay)






(* check all exceeding candidates for applicability. *)
let check_exceeding (candidates: candidates) : unit =
  if not (candidates.cd_unprocessed_size = 0) then
    failwith "Selection_split.check_exceeding";

  (* enter exceeding checking mode *)
  candidates.cd_check_exceeding <- true;

  (* recompute for each context literal all candidates in chronological order *)
  begin
    try
      Context.iter
	(fun element ->
	   if is_marked_exceeding candidates element then
	     Problem_literals.compute_for_element candidates.cd_problem_literals element Context_unifier.Split;
	)
	candidates.cd_context;

      (* build all unprocessed exceeding candidates *)
      build_unprocessed ~force:true candidates;
    with
      | EXCEEDING_EXHAUSTED ->
	  ()
  end;

  (* unprocessed candidates might be in an inconsistent state due to EXCEEDING_EXHAUSTED.
     as it can only contain recomputed candidates, it can be savely emptied. *)
  clear_unprocessed candidates;

  (* leave exceeding checking mode *)
  candidates.cd_check_exceeding <- false





(*** backtrack ***)

(* backtrack right splits *)
let backtrack_right_splits (candidates: candidates) : unit =
  (* :TODO: save right splits for random jumping. *)

  (* right splits can't be kept for random jumping *)
  if Config.jumping candidates.cd_config then begin
    candidates.cd_right_splits <- [];
    candidates.cd_applied_right_splits <- [];
  end

  else begin
    (* move the still applicable applied right splits
       which have been applied in a retracted choice point
       back to the unapplied ones, preserving the split order *)
    let rec backtrack_right_splits' applied_splits =
      match applied_splits with
	| [] ->
	    candidates.cd_applied_right_splits <- []

	| ((_, _, _, explanation) as applied, applied_at) :: tail ->
	    (* ignore invalid right splits *)
	    if
	      Tools.array_exists
		(fun literal -> Context.element_for_literal candidates.cd_context literal = None)
		explanation
	    then
	      backtrack_right_splits' tail

	    (* move valid but retracted right splits *)
	    else if State.is_choice_point_invalid applied_at then
	      candidates.cd_right_splits <- applied :: candidates.cd_right_splits

	    (* all right splits from now were applied in still valid choice points *)
	    else
	      candidates.cd_applied_right_splits <- applied_splits		
    in
      backtrack_right_splits' candidates.cd_applied_right_splits;
  end



(* backtrack all invalid candidates *)
let backtrack_invalid (candidates: candidates) : unit =

  (* a candidate is invalidated by a specific context element.
     now, if this element has been retracted,
     the candidate is moved to the unprocessed candidate buffer.
     but, these are processed if the buffer is full,
     thus potentially moving the candidate back to the invalid candidates.

     therefore, the move to the unprocessed candidates,
     and the cleaning of the invalid candidates must be done in sequence.
  *)
  let rec backtrack_invalid still_invalid now_unprocessed invalid =
    match invalid with
      | [] ->
	  candidates.cd_invalid <- still_invalid;
	  List.iter (add_unprocessed candidates) now_unprocessed

      | (raw_context_unifier, conflicting_choice_point) as candidate :: tail ->
	  (* candidate does no longer exist *)
	  if Context_unifier.is_raw_context_unifier_invalid raw_context_unifier then
	    backtrack_invalid still_invalid now_unprocessed tail
	     
	 (* candidate is no longer invalidated by the context *)
	 else if State.is_choice_point_invalid conflicting_choice_point then
	   backtrack_invalid still_invalid (raw_context_unifier :: now_unprocessed) tail

	 (* candidate still not applicable *)
	 else
	   backtrack_invalid (candidate :: still_invalid) now_unprocessed tail
  in
    backtrack_invalid [] [] candidates.cd_invalid
  


(* backtrack all applied left splits.
   store the currently rectracted left split as a the next right split to apply. *)
let backtrack_added (candidates: candidates) (retracted: choice_point) (explanation: literal array) : unit =
  candidates.cd_added <-
    List.find_all
    (fun (candidate, split_literal, index, choice_point) ->
       (* this is the left split which is currently being retracted *)
       if State.choice_point_equal choice_point retracted then begin
	 (* reenter the candidate so that it is available after backtracking.

	    it is usually immediately invalidated by applying the corresponding right split,
	    but after further backtracking it might become applicable again.

	    in the case of Const.ignore_skolem_literals it is reused
	    to select another remainder literal.
	 *)
	 add_valid candidates candidate;

	 candidates.cd_right_splits <- (candidate, split_literal, index, explanation) :: candidates.cd_right_splits;
	 false;
       end
       
       (* candidate does no longer exist *)
       else if Context_unifier.is_raw_context_unifier_invalid candidate.ca_raw_context_unifier then
	 false
	   
       (* candidate is no longer invalidated by the context *)
       else if (State.is_choice_point_invalid choice_point) then begin
	 (* add it directly to the 'valid' ones
	    to avoid recomputation of the candidate.
	    it will be rechecked on selection anyways. *)
	 add_valid candidates candidate;
	 false
       end
	 
       (* candidate still not applicable *)
       else
	 true
    )
    candidates.cd_added
  


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



(* backtrack all candidates *)
let backtrack (candidates: candidates) (retracted: choice_point) (explanation: literal list) : unit =
  let explanation =
    Array.of_list (
    Tools.list_remove_first
      (fun literal -> Term.literal_equal literal (State.left_split_literal retracted))
      explanation
    )
  in
  (* unprocessed and valid candidates are not backtracked,
     retracted ones are dropped when build *)
  backtrack_invalid candidates;

  backtrack_right_splits candidates;

  (* backtrack applied splits, store the retracted one as a right split
     -> backtrack_right_splits must be done before. *)
  backtrack_added candidates retracted explanation;

  (* assure that the right split of the currently retracted split has been found. *)
  if Const.debug then begin
    match candidates.cd_right_splits with
      | [] ->
	  failwith "Selection_split.backtrack: no right split found"
	    
      | _ ->
	  ()
  end;

  backtrack_exceeding candidates;


