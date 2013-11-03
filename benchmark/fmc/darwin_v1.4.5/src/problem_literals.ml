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
type binding = Subst.binding
type subst = Subst.subst
type choice_point = State.choice_point
type state = State.state
type context = Context.context
type context_partner = Context_unifier.context_partner
type input_partner = Context_unifier.input_partner
type context_unifier_space = Context_unifier.context_unifier_space
type 'data stack = 'data Stack.stack


(* binding database *)
module Bindings =
  Hashtbl.Make (
    struct
      type t = binding

      let equal =
	Subst.binding_equal

      let hash (binding: binding) : int =
	let var_hash =
	  Var.hash_of_var binding.Subst.sb_var.Subst.sv_var
	and term_hash =
	  Term.hash_of_term binding.Subst.sb_term.Subst.st_term
	in
	  var_hash + (term_hash * 131)

    end
  )



(* the clauses using this input partner,
   i.e. containing this input literal at the given position *)
type element = {
  (* the clause literal with its position *)
  input_partner: input_partner;

  (* the clauses containing this input partner *)
  mutable spaces: context_unifier_space list;
}

(* map from offset to element *)
module Elements =
  Map.Make (
    struct
      type t = int

      let compare (first: int) (second: int) : int =
	Tools.compare_int first second
    end
  )

(* a clause literal.
   variants, i.e. clause literals with at different positions
   in the clause are reached via elements. *)
type problem_literal = {
  (* the problem / clause literal *)
  literal: literal;

  (* variants of the problem literal, i.e. pairing with different offsets *)
  mutable elements: element Elements.t;

  (* the context literal with this id subsumes this literal,
     -1 if not subsumed *)
  mutable subsumed_at: int;
}






(* problem literal accessed by literal. *)
module LiteralTable = Term.LiteralTable

(* (bunch of) problem literals accessed by sign and predicate. *)
(*module Index = Term.LiteralTypeTable*)

class problem_literal_data =
object
  method is_equal (first: problem_literal) (second: problem_literal) : bool =
    Term.literal_equal first.literal second.literal
      
  method to_string (data: problem_literal) : string =
    "problem_literal: " ^ Term.literal_to_string data.literal
end

type predicate_index = problem_literal Term_indexing.predicate_index


type problem_literals = {
  (* environment *)
  config: config;
  bound: bound;
  statistic: statistic;
  state: state;
  context: context;

  (* access to problem literals by literal - used during creation *)
  problem_literals: problem_literal LiteralTable.t;

  (* access to problem literals by sign and predicate -
     used when checked against new context literals
     to quickly ignore literals with wrong sign and predicate *)
  (*index: (problem_literal list) Index.t;*)
  index: problem_literal Term_indexing.index;

  (* database for the bindings used in the substitution
     between the problem literals and the context partners (cp_subst).
     this is in general a slight performance loss,
     but can in some cases significantly decrease the memory usage (>>100MB),
     esp. for the PUZ TPTP horn problems.

     the first bindings has been registered in the given choice point,
     needed for removal on backtracking.
  *)
  bindings: (binding * choice_point) Bindings.t;


  (* the current subsumption state of all clauses.
     the state of a clause, i.e. context_unifier_space,
     is stored at the index context_unifier_space.Context_unifier.cus_id.
     the entry is the id of the subsuming context literal,
     i.e. Context.el_id,
     or -1, if it is not subsumed.

     instead of simply storing the subsumption status
     within the context_unifier_space,
     this whole ugly indirection business is just done for performance,
     i.e. to avoid touching the memory of subsumed clauses.
     for dynamically added clauses, e.g. lemmas, this can be quite important.

     :TODO: try hashtbl instead
  *)
  mutable subsumed_clauses: int array;

  (* the number of currently cached partial context unifiers,
     i.e. substitutions stored in some context_partner.cp_partial_context_unifier *)
  mutable cached_partial_context_unifiers: int;
}





(*** binding database ***)  

let register_binding (problem_literals: problem_literals) (binding: binding) : binding =
  try
    fst (Bindings.find problem_literals.bindings binding)
  with
    | Not_found ->
	Bindings.add problem_literals.bindings
	  binding
	  (binding, State.active_choice_point problem_literals.state);
	binding


let register_subst (problem_literals: problem_literals) (subst: subst) : subst =
  Subst.map (register_binding problem_literals) subst




(*** storing partial context unifiers ***)

(* if store_subst is true,
   the context unifier is stored even if the cache limit is exceeded.
   this is useful for resolving unifiers,
   as these are smaller than regular unifiers (see resolve) *)
let store_partial_context_unifier (problem_literals: problem_literals) (subst: subst) : subst option =
  (* always store empty substs without increasing the counter *)
  if Subst.is_empty subst then begin
    Some subst
  end

  else if
    (* store if the limit is not reached *)
    problem_literals.cached_partial_context_unifiers < Const.max_cached_partial_context_unifiers
    ||
    (* store all short unifiers *)
    (Subst.length subst < 2)
  then begin
    problem_literals.cached_partial_context_unifiers <- problem_literals.cached_partial_context_unifiers + 1;

    (* drop bindings to universal context variables not used anywhere.
       may speed up partial context unifier computation. *)
    let pruned_subst =
      (*Subst.remove_universal_context_vars subst*)
      Subst.remove_context_var_renamings subst
    in
      Some (register_subst problem_literals pruned_subst)
  end
			     
  else
    None



(*** creation ***)



let create (config: config) (bound: bound) (statistic : statistic) (state: state) (context: context) : problem_literals =
  {
    config = config;
    bound = bound;
    statistic = statistic;
    state = state;
    context = context;
    problem_literals = LiteralTable.create 16;
    (*index = Index.create 8;*)
    index = Discrimination_tree.create_index false (new problem_literal_data);
    bindings = Bindings.create 128;
    subsumed_clauses = Array.create 32 (-1);
    cached_partial_context_unifiers = 0;
  }


(* creates the initial queue of context partners for a literal.
   i.e. either no initial partner or -v for a positive literal. *)
let create_initial_context_partners (problem_literals: problem_literals)
    (input_partner: input_partner) : unit =

    if 
      (* in the horn case split never happens,
	 so only assert or close context unifiers are of interest.
	 thus -v which produces non-empty remainders is not needed. *)
      
      (Config.is_horn problem_literals.config)
      ||
      (* -v or +v may be used -
	 and is only applicable if the literal has the opposite sign.  *)
	input_partner.Context_unifier.ip_literal.Term.sign = Config.plus_v problem_literals.config;
    then
      ()

    else begin
      (* dummy values for -v/+v:
	 just provide a parameter which is bound to a non-parameter term.
	 
	 as the remainder literals are built from the variables of the input clause
	 and not from the parameters of the context literals,
	 this binding is never applied.
	 it is only checked in order to determine its remainder status,
	 so a binding to any non-parameter is sufficient *)
      let subst =
	Subst.set' ~recompute:false ~p_preserving:false
	  Subst.empty
	  Term.v_par (Subst.context_literal_offset input_partner.Context_unifier.ip_index)
	  Term.v_term (Subst.context_literal_offset input_partner.Context_unifier.ip_index);
      in
      let v_cp_element = 
	if Config.plus_v problem_literals.config then
	  Context.plus_v_element
	else
	  Context.minus_v_element
      in
      let registered =
	register_subst problem_literals subst
      in
      let context_partner =
	Context_unifier.create_context_partner v_cp_element (Some registered) false
      in
	Stack.push input_partner.Context_unifier.ip_context_partners context_partner
    end



let create_input_partner (problem_literals: problem_literals) (literal: literal) (index: int) : input_partner =
  let vars =
    List.map
      (fun var ->
	 Subst.make_var var Subst.input_literal_offset
      )
      (Term.vars_of_literal literal)
  in
  let input_partner =
    Context_unifier.create_input_partner
      index literal vars
  in
    create_initial_context_partners problem_literals input_partner;
    input_partner

let create_context_partner (problem_literals: problem_literals)
    (_input_partner: input_partner) (context_element: Context.element) (subst: subst) : context_partner =
  let p_preserving =
    match context_element.Context.el_pars with
      | [] -> true

      | _ ->
	  not (Context_unifier.is_remainder' subst)
  in
    Context_unifier.create_context_partner
      context_element (store_partial_context_unifier problem_literals subst) p_preserving


(* clones a context partner of one input partner
   to a context partner of another input partner,
   i.e. replaces the corresponding offsets.
   
   does not change the original subst
*)
let clone_context_partner (problem_literals: problem_literals)
    (context_partner: context_partner) (old_input_partner: input_partner) (new_input_partner: input_partner) :
    context_partner =

  let partial_context_unifier =
    match context_partner.Context_unifier.cp_partial_context_unifier with
      | None ->
	  None

      | Some subst ->
	  store_partial_context_unifier
	    problem_literals
	    (
	      Subst.replace_offset
		subst
		(Subst.context_literal_offset old_input_partner.Context_unifier.ip_index)
		(Subst.context_literal_offset new_input_partner.Context_unifier.ip_index)
	    )
  in
    Context_unifier.create_context_partner
      context_partner.Context_unifier.cp_element
      partial_context_unifier
      context_partner.Context_unifier.cp_empty_remainder









(*** subsumption ***)

(* clause subsumed wrt. to the context? *)
let is_space_subsumed (problem_literals: problem_literals)
    (context_unifier_space: context_unifier_space) : bool =

  problem_literals.subsumed_clauses.(context_unifier_space.Context_unifier.cus_id) != -1


(* clause subsumed wrt. to the context element? *)
let is_space_subsumed_at (problem_literals: problem_literals) 
  (context_unifier_space: context_unifier_space) (context_element: Context.element) : bool =

  problem_literals.subsumed_clauses.(context_unifier_space.Context_unifier.cus_id) != -1
  &&
  (
    problem_literals.subsumed_clauses.(context_unifier_space.Context_unifier.cus_id)
    <
    context_element.Context.el_id
  )


(* literal subsumed wrt. the context element?

   directly:
   - true: this literal is subsumed
   - false: all clauses containing this literal are subsumed,
     so in effect this literal is 'subsumed' as well
 *)
let is_problem_literal_subsumed_at ~(directly: bool) (problem_literals: problem_literals)
    (problem_literal: problem_literal) (context_element: Context.element) : bool =

  (* unfortunately, directly has to be used whenever the clause set is not fixed.
     that is, currently, if lemmas are used.
  *)
  let directly = true in

  (* directly subsumed? *)
  if problem_literal.subsumed_at = -1 then begin
    (* no *)
    if directly then
      false

    (* indirectly subsumed? *)
    else
      try
	(* for all variants of the literal (offsets) ... *)
	Elements.iter
	  (fun _ element ->
	     (* ... and all containing clauses ... *)
	     List.iter
	       (fun space ->
		  (* ... check subsumption *)
		  if not (is_space_subsumed_at problem_literals space context_element) then
		    raise Exit
	       )
	       element.spaces;
	  )
	  problem_literal.elements;
	true
      with
	| Exit ->
	    false
  end

  else begin
    (* subsumed by this context literal? *)
    problem_literal.subsumed_at < context_element.Context.el_id
  end




let mark_space_subsumed (problem_literals: problem_literals)
    (context_unifier_space: context_unifier_space) (context_element: Context.element) : unit =

  if not (is_space_subsumed_at problem_literals context_unifier_space context_element) then begin
    Statistic.inc_subsume problem_literals.statistic;
    problem_literals.subsumed_clauses.(context_unifier_space.Context_unifier.cus_id) <- context_element.Context.el_id
  end

let check_space_subsumed (problem_literals: problem_literals)
    (context_unifier_space: context_unifier_space) : unit =

  let get_best acc element =
    match acc with
      | Some best when
	  best.Context.el_id <= element.Context.el_id ->
	  (* not better than previous subsuming literal *)
	  acc
	    
      | _ ->
	  (* best subsuming context literal up to now. *)
	  Some element
  in
  (* find best subsuming in compacted context *)
  let subsuming =
    List.fold_left
      (fun acc literal ->
	 List.fold_left
	   (fun acc element ->
	      get_best acc element
	   )
	   acc
	   (Context.find_all_subsuming problem_literals.context literal)
      )
      None
      context_unifier_space.Context_unifier.cus_clause
  in
  (* find best subsuming in compacted elements *)
  let subsuming =
    Context.fold_compacted
      (fun best element ->
	 (* is this a subuming element? *)
	 if
	   List.exists
	     (fun literal ->
		Unification.is_literal_generalization ~p_preserving:true
		  element.Context.el_literal
		  literal
	     )
	     context_unifier_space.Context_unifier.cus_clause
	 then
	   get_best best element

	 else
	   best
      )
      subsuming
      problem_literals.context
  in
    match subsuming with
      | None -> ()
      | Some element ->
	  mark_space_subsumed problem_literals context_unifier_space element


(* the problem literal subsumes the context literal,
   so mark the corresponding clauses as subsumed. *)
let subsume_problem_literal (problem_literals: problem_literals)
    (problem_literal: problem_literal) (context_element: Context.element) : unit =

  if is_problem_literal_subsumed_at ~directly:false problem_literals problem_literal context_element then
    ()
      
  else begin
    (* mark all clauses using this problem literal as subsumed *)
    Elements.iter
      (fun _ element ->
	List.iter
	  (fun space ->
	    mark_space_subsumed problem_literals space context_element
	  )
	  element.spaces;
      )
      problem_literal.elements;
    
    (* mark the problem literal as subsumed *)
    problem_literal.subsumed_at <- context_element.Context.el_id;

    (* no backtracking in horn - so this stays subsumed forever
       and all context partners may be forgotten.
       not to be done for stable derivations, all candidates must be recomputed. *)
    if
      not Const.stable_derivation
      &&
      Config.is_horn problem_literals.config
    then begin
      Elements.iter
	(fun _ element ->
	   Stack.clear element.input_partner.Context_unifier.ip_context_partners;
	)
	problem_literal.elements
    end;
  end


(* subsume all clauses with this given context literal *)
let subsume (problem_literals: problem_literals) (context_element: Context.element) : unit =
  if 
    (Config.subsume problem_literals.config)
    &&
    (* only universal literals can subsume input literals as these are always universal *)
    (match context_element.Context.el_pars with
       | [] -> true
       | _ -> false
    )
  then begin
    let subsume_candidates =
      let index =
	problem_literals.index#find context_element.Context.el_literal
      in
	index#find_all_instances ~p_preserving:true context_element.Context.el_literal.Term.atom
    in
      List.iter
	(fun problem_literal ->
	   subsume_problem_literal problem_literals problem_literal context_element
	)
	subsume_candidates;
  end






(*** resolve ***)

(* is the (input partner) literal resolved wrt. the context element? *)
let is_input_partner_resolved_at (input_partner: input_partner) (context_element: Context.element) : bool =  
    match input_partner.Context_unifier.ip_resolved with
      | Some resolving_partner when
	  resolving_partner.Context_unifier.cp_element.Context.el_id
	  <
	  context_element.Context.el_id
	  ->
	  true

      | _ ->
	  false


(* mark the problem literal as resolved by this context literal *)
let mark_resolved (problem_literals: problem_literals)
  (input_partner: input_partner) (resolving_element: Context.element): context_partner =

  Statistic.inc_resolve problem_literals.statistic;

  let resolving_partner =		
    create_context_partner problem_literals input_partner resolving_element Subst.empty
  in
    Context_unifier.set_resolved input_partner (Some resolving_partner);

  (* no backtracking in horn - so this stays resolved forever
     and some entries can be removed for garbage collection.
     not to be done for stable derivations, all candidates must be recomputed. *)
  if
    not Const.stable_derivation
    &&
    Config.is_horn problem_literals.config
  then begin
    Stack.clear input_partner.Context_unifier.ip_context_partners;
  end;

  resolving_partner



(* checks the resolve inference rule against the problem literal and the context literal.
   
   as this resolve amounts to a p-preserving subsumption from context to problem literal,
   the substitution contains only bindings of this context literal's variables,
   as all context literals are variable disjoint.
   furthermore, it is p-preserving, therefore the context literal
   does not produce a remainder literal.
   but this is all these bindings could be used for,
   as the actual instance is computed by applying the substitution to the problem literal.
   thus, the substitution can be completely ignored and the empty substitution can be used instead.

   This optimization can vastly improve the speed of merging substitutions for some problems.
*)
let resolve (problem_literals: problem_literals)
  (input_partner: input_partner) (context_element: Context.element) : context_partner option =

  if not (Config.resolve problem_literals.config) then
    None

  else if
    context_element.Context.el_literal.Term.sign
    =
    input_partner.Context_unifier.ip_literal.Term.sign
  then
    None

  else begin
    let context_term =
      context_element.Context.el_literal.Term.atom
    and problem_term =
      input_partner.Context_unifier.ip_literal.Term.atom
    in
      (* variants? *)
      if Term.are_terms_variants context_term problem_term then begin
	Some (mark_resolved problem_literals input_partner context_element)
      end
	
      (* subsumed? this implies Resolve,
	 as the effect on the rest clause can only be a variable remaining *)
      else begin
	try
	  ignore (
	    Unification.match_terms
	      ~recompute:false
	      ~p_preserving:true
	      context_term
	      (Subst.context_literal_offset input_partner.Context_unifier.ip_index)
	      problem_term
	      Subst.input_literal_offset
	      : subst);
	    Some (mark_resolved problem_literals input_partner context_element)
	    
	with
	  | Unification.UNIFICATION_FAIL ->
	      None
      end
  end








(*** registration ***)

(* copy the initial context partner *)
let copy_initial_context_partners
    (problem_literals: problem_literals) (problem_literal: problem_literal) (input_partner: input_partner) : unit =
  Elements.iter
    (fun _ element ->
       let old_input_partner =
	 element.input_partner
       in

	 (* copy resolving partner *)
	 begin
	   match old_input_partner.Context_unifier.ip_resolved with
	     | Some old_resolving_partner ->
		 ignore (
		   mark_resolved problem_literals input_partner old_resolving_partner.Context_unifier.cp_element
		     : context_partner)

	     | None ->
		 ()
	 end;

	 Stack.clear input_partner.Context_unifier.ip_context_partners;
	 Stack.iter
	   (fun context_partner ->
	      Stack.push input_partner.Context_unifier.ip_context_partners
		(clone_context_partner problem_literals context_partner old_input_partner input_partner)
	   )
	   old_input_partner.Context_unifier.ip_context_partners
	 
    )
    problem_literal.elements

(* compute all partial context unifiers with all context literals
   for this input_partner *)
let compute_initial_context_partners
    (problem_literals: problem_literals) (problem_literal: problem_literal) (input_partner: input_partner) : unit =

  (* check if the literal is resolved by a context literal *)
  if Config.resolve problem_literals.config then begin
    let resolve_candidates =
      Context.find_all_subsuming problem_literals.context
	(Term.request_negated_literal ~insert_db:false input_partner.Context_unifier.ip_literal)
    in
    (* need also to take compacted elements into account.
       of course, if there are no resolving elements in the context,
       then there can't be any compacted resolving elements either. *)
    let resolve_candidates =
      match resolve_candidates with
	| [] -> resolve_candidates
	| _ ->
	    Context.fold_compacted
	      (fun acc element ->
		 if
		   (
		     element.Context.el_literal.Term.sign
		     !=
		     input_partner.Context_unifier.ip_literal.Term.sign
		   )
		   &&
		     Unification.is_term_generalization ~p_preserving:true
		     element.Context.el_literal.Term.atom
		     input_partner.Context_unifier.ip_literal.Term.atom
		 then
		   element :: acc
		 else
		   acc
	      )
	      resolve_candidates
	      problem_literals.context
    in
    let oldest_resolving =
      List.fold_left
	(fun acc candidate ->
	   (* ignore context literals added after the literal has been subsumed *)
	   if is_problem_literal_subsumed_at ~directly:true problem_literals problem_literal candidate then
	     acc
	       
	   else
	     match acc with
	       | Some best when
		   best.Context.el_id <= candidate.Context.el_id ->
		   (* not better than previous resolving literal *)
		   acc
		     
	       | _ ->
		   (* best resolving context literal up to now. *)
		   Some candidate
	)
	None
	resolve_candidates
    in
      begin
	match oldest_resolving with
	  | Some resolving_element ->
	      ignore (mark_resolved problem_literals input_partner resolving_element : context_partner);
		
	  | None ->
	      ()
      end;
  end;

  (* for pure lemma problem literals only compute resolving context partners *)
  (* now compute all partial context unifiers *)
  let candidates =
    Context.find_all_unifiable
      problem_literals.context (Term.request_negated_literal ~insert_db:false input_partner.Context_unifier.ip_literal)
  in
    (* check each unifying context element *)
    List.iter
      (fun context_element ->
	 if is_problem_literal_subsumed_at ~directly:true problem_literals problem_literal context_element then
	   ()
	    
	 else if is_input_partner_resolved_at input_partner context_element then
	   ()

	 else try
	   (* recreate the unifier - put context literal first,
	      as it is then more likely that universal context variables are bound. *)
	   let subst =
	     Unification.unify_terms ~recompute:true
	       context_element.Context.el_literal.Term.atom
	       (Subst.context_literal_offset input_partner.Context_unifier.ip_index)
	       input_partner.Context_unifier.ip_literal.Term.atom
	       Subst.input_literal_offset
	   in
	     
	   let context_partner =
	     create_context_partner problem_literals input_partner context_element subst
	   in
	     (* and add it *)
	     Stack.push input_partner.Context_unifier.ip_context_partners context_partner;
	 with
	   | Unification.UNIFICATION_FAIL ->
	       failwith "Problem_literals.compute_initial_partial_context_unifiers"
      )
      candidates;

    (* again, also need also to take compacted elements into account. *)
    if Stack.size input_partner.Context_unifier.ip_context_partners > 0 then begin
      Context.iter_compacted
	(fun context_element ->
	   if is_problem_literal_subsumed_at ~directly:true problem_literals problem_literal context_element then
	     ()
	       
	   else if is_input_partner_resolved_at input_partner context_element then
	     ()
	       
	 else try
	   (* recreate the unifier - put context literal first,
	      as it is then more likely that universal context variables are bound. *)
	   let subst =
	     Unification.unify_terms ~recompute:false
	       context_element.Context.el_literal.Term.atom
	       (Subst.context_literal_offset input_partner.Context_unifier.ip_index)
	       input_partner.Context_unifier.ip_literal.Term.atom
	       Subst.input_literal_offset
	   in
	     
	   let context_partner =
	     create_context_partner problem_literals input_partner context_element subst
	   in
	     (* and add it *)
	     Stack.push input_partner.Context_unifier.ip_context_partners context_partner;
	 with
	   | Unification.UNIFICATION_FAIL ->
	       (* failure is fine here *)
	       ()
	)
	problem_literals.context
    end;

    (* sort these to get a queue suitable for chronological backtracking *)
    Stack.sort
      (fun x y ->
	 compare
	   x.Context_unifier.cp_element.Context.el_id
	   y.Context_unifier.cp_element.Context.el_id
      )
      input_partner.Context_unifier.ip_context_partners

(* find all partial context unifiers with all context literals for this input_partner *)
let set_initial_context_partners
    (problem_literals: problem_literals) (problem_literal: problem_literal) (input_partner: input_partner) : unit =

  (* if any other variant of this input literal exists,
     reuse it's context partners. otherwise, compute them fresh. *)
  if Elements.is_empty problem_literal.elements then begin
    compute_initial_context_partners problem_literals problem_literal input_partner
  end
  else begin
    copy_initial_context_partners problem_literals problem_literal input_partner
  end





(* add the problem literal to the index *)

let register_to_index (problem_literals: problem_literals) (problem_literal: problem_literal) : unit =
  let index =
    problem_literals.index#find problem_literal.literal
  in
    index#add problem_literal.literal.Term.atom problem_literal


let get_problem_literal (problem_literals: problem_literals) (literal: literal) : problem_literal =
  try
    LiteralTable.find problem_literals.problem_literals literal
  with
    | Not_found ->
	(* create a new problem literal
	   and update index structures to refer to it *)
	let new_problem_literal = {
	  literal = literal;
	  elements = Elements.empty;
	  subsumed_at = -1;
	}
	in
	  
	  if Config.subsume problem_literals.config then begin
	    (* check if the problem literal is subsumed by a context literal *)
	    let subsumption_candidates =
	      Context.find_all_subsuming problem_literals.context literal
	    in
	      List.iter
		(fun element ->
		   if
		     (* first subsuming element *)
		     new_problem_literal.subsumed_at = -1
		     ||
		     (* older subsuming element *)
		     element.Context.el_id < new_problem_literal.subsumed_at
		   then
		     new_problem_literal.subsumed_at <- element.Context.el_id
		)
		subsumption_candidates;
	  end;
	  
	  (* add new problem literal to indexes *)
	  
	  LiteralTable.add problem_literals.problem_literals literal new_problem_literal;
	  register_to_index problem_literals new_problem_literal;
	  new_problem_literal


let register_literal
    (problem_literals: problem_literals) (literal: literal) (index: int) : input_partner =
  (* find the corresponding problem literal *)
  let problem_literal =
    get_problem_literal problem_literals literal
  in

  (* find the corresponding input_partner *)
  let element =
    try
      Elements.find index problem_literal.elements
    with
      | Not_found ->
	  (* first occurance of this literal/index: create it *)
	  let new_input_partner =
	    create_input_partner problem_literals literal index
	  in
	  let new_element = {
	    input_partner = new_input_partner;
	    spaces = [];
	  }
	  in
	    set_initial_context_partners problem_literals problem_literal new_input_partner;

	    (* add new input partner *)
	    problem_literal.elements <- Elements.add index new_element problem_literal.elements;
	    new_element
  in
    element.input_partner





(* a new clause which doesn't fit into the subsumed marker array?
   if so, extend the array. *)
let extend_subsumed_clauses (problem_literals: problem_literals) (space: context_unifier_space) : unit =
  if Array.length problem_literals.subsumed_clauses <= space.Context_unifier.cus_id then begin
    if space.Context_unifier.cus_id >= Sys.max_array_length - 1 then begin
      raise (Const.NO_SOLUTION "Problem_literals.register_clause_literal: OVERFLOW")
    end;
    
    let new_size =
      let desired_size = 
	Tools.max_int (space.Context_unifier.cus_id + 1) (2 * (Array.length problem_literals.subsumed_clauses))
      in
	(* obey the hard limit of the max. array size *)
	min Sys.max_array_length desired_size
    in
    let new_array =
      Array.make new_size (-1)
    in
      Array.blit problem_literals.subsumed_clauses 0 new_array 0 (Array.length problem_literals.subsumed_clauses);
      problem_literals.subsumed_clauses <- new_array;
  end


let register_input_partner
    (problem_literals: problem_literals) (input_partner: input_partner) (space: context_unifier_space) : unit  =

  (* ensure that this clause fits into the subsumed marker array *)
  extend_subsumed_clauses problem_literals space;

  let literal =
    input_partner.Context_unifier.ip_literal
  in
  let index =
    input_partner.Context_unifier.ip_index
  in
  let problem_literal =
    try
      LiteralTable.find problem_literals.problem_literals literal
    with
      | Not_found ->
	  failwith "register_clause_literal: problem_literal"
  in

  let element =
    try
      Elements.find index problem_literal.elements
    with
      | Not_found ->
	  failwith "register_clause_literal: element"
  in
    element.spaces <- space :: element.spaces


let unregister_space (problem_literals: problem_literals) (space: context_unifier_space) : unit  =
  Array.iter
    (fun input_partner ->
       let literal =
	 input_partner.Context_unifier.ip_literal
       in
       let index =
	 input_partner.Context_unifier.ip_index
       in
       let problem_literal =
	 try
	   LiteralTable.find problem_literals.problem_literals literal
	 with
      | Not_found ->
	  failwith "unregister_clause_literal: problem_literal"
       in
	 
       let element =
	 try
	   Elements.find index problem_literal.elements
	 with
	   | Not_found ->
	       failwith "unregister_clause_literal: element"
       in
	 (* remove space from input partner *)
	 element.spaces <- List.find_all (fun space' -> space != space') element.spaces;
	 
	 (* remove input partner if no space left *)
	 begin
	   match element.spaces with
	     | [] ->
		 problem_literal.elements <- Elements.remove index problem_literal.elements;
		 
	 (* too cautious in unregistering bindings.
	    less performant, less memory wasted. *)
	 begin
	   Stack.iter
	     (fun context_partner ->
		match context_partner.Context_unifier.cp_partial_context_unifier with
		  | None ->
		      ()

		  | Some subst ->
		      Subst.iter (Bindings.remove problem_literals.bindings) subst
	     )
	     input_partner.Context_unifier.ip_context_partners;
	   match input_partner.Context_unifier.ip_resolved with
	     | None ->
		 ()

	     | Some context_partner ->
		 begin
		   match context_partner.Context_unifier.cp_partial_context_unifier with
		     | None ->
			 ()
			   
		     | Some subst ->
			 Subst.iter (Bindings.remove problem_literals.bindings) subst
		 end
	 end

	     | _ ->
		 ()
	 end;
	 
	 (* remove problem literal if no input partner left *)
	 if Elements.is_empty problem_literal.elements then begin
	   LiteralTable.remove problem_literals.problem_literals literal;
	   let index =
	     problem_literals.index#find problem_literal.literal
	   in
	     if not (index#remove problem_literal.literal.Term.atom (Some problem_literal)) then
	       failwith "unregister_clause_literal: problem literal 2"	       
	 end;
    )
    space.Context_unifier.cus_input_partners





(*** reorder ***)

(* sort input literals by increasing number of context partners.
   keep resolved literals at the end.
   thus less partial context unifier extending steps need to be done,
   as the first considered literals have fewer context partners
   and thus branching is lower in the top part of the search tree.
*)

let move_increased_input_literal (context_unifier_space: context_unifier_space)
    (move: input_partner) (config: config) : unit =
  let input_partners =
    context_unifier_space.Context_unifier.cus_input_partners
  in
  let ordering =
    context_unifier_space.Context_unifier.cus_input_partners_ordering
  in
  let move_index =
    let rec inverse i =
      if i >= Array.length ordering then
	failwith "move_increased_input_literal"

      else if ordering.(i) = move.Context_unifier.ip_index then
	i

      else
	inverse (i + 1)
    in
      inverse 0	
  in

  (* find the index of the next unresolved clause literal
     with at least the same number of context partners as the literal to move. *)
  let rec find_position i =
    (* move to the end *)
    if i >= Array.length input_partners then
      i - 1

    (* skip a resolved literal *)
    else if
      match input_partners.(ordering.(i)).Context_unifier.ip_resolved with
	| None -> false
	| _ -> true
    then
      find_position (i + 1)

    (* the next literals queue is not bigger, so stop here and move to the previous position.
       as the partner queue of only one clause literal is increased at a time,
       this is ok.
    *)
    else if
      Stack.size input_partners.(ordering.(move_index)).Context_unifier.ip_context_partners
      <=
      Stack.size input_partners.(ordering.(i)).Context_unifier.ip_context_partners
    then
      i - 1

    else (* new > compared *)
      find_position (i + 1)
  in
  let new_position =
    find_position (move_index + 1)
  in

  (* move literal to its new position by swapping.
     as its partner number has been increased by only 1,
     and no other partner queue has been increased,
     the swapped element is moved only over other problem literals
     with the same queue sizes.
     thus, swapping suffices to keep the ordering intact.
  *)
  let swap =
    ordering.(new_position)
  in
    ordering.(new_position) <- ordering.(move_index);
    ordering.(move_index) <- swap;


  (* check that the input literal order is intact.
     for horn problems this is a little bit tricky,
     as for resolved problem literal their context partner queues are cleared (mark_resolved).
     this means, for all input literals corresponding to a problem literal
     when the first input literal is resolved,
     even though the context unifiers for the others may not have been computed yet (add_to_problem_literal).
     this might mess up the order, if a context partner queue is emptied
     _before_ it is considered to be extended with the new context literal.
  *)
  if Const.debug && not (Config.is_horn config) then begin
    for i = 1 to Array.length input_partners - 1 do
      if
	(
	  (
	    Stack.size input_partners.(ordering.(i)).Context_unifier.ip_context_partners
	    >=
	    Stack.size input_partners.(ordering.(i - 1)).Context_unifier.ip_context_partners
	  )
	  ||
	    (match input_partners.(ordering.(i)).Context_unifier.ip_resolved with
	       | Some _ -> true
	       | None -> false
	    )
	  ||
	    (match input_partners.(ordering.(i - 1)).Context_unifier.ip_resolved with
	       | Some _ -> true
	       | None -> false
	    )
	)
      then
	()
	  
      else begin
	print_endline ("MOVED INDIRECTION: " ^ string_of_int move_index
		       ^ " to " ^ string_of_int new_position);
	for i = 0 to Array.length input_partners - 1 do
	  print_endline (
	    string_of_int i ^ ": " ^ string_of_int ordering.(i) ^ ": " ^
	      (match input_partners.(ordering.(i)).Context_unifier.ip_resolved with
		 | Some _ ->
		     "RESOLVED"
		 | None ->
		     string_of_int (Stack.size input_partners.(ordering.(i)).Context_unifier.ip_context_partners)
	      )
	  );
	done;
	failwith "Problem_literals.move_increased_input_literal: input partner order not intact."
      end
    done;
  end




(* keep the ordering of clause literals in increasing order of context partners,
   so move the input partner which has a new context partner accordingly *)
let move_updated_input_literal
  (context_unifier_space: context_unifier_space)
  (input_partner: input_partner) (context_partner: context_partner) (config: config)
  : unit =

  if is_input_partner_resolved_at input_partner context_partner.Context_unifier.cp_element then
    (* don't move a resolved literal.
       doing the reordering so that all resolved literals are at the end of the clause
       and can be skipped in one row is in general more expensive
       than not reordering them, and just skipping them when they appear.
    *)
    ()
  else
    move_increased_input_literal context_unifier_space input_partner config







(*** add a literal ***)


(* pair the new context literal with the clause literals one by one
   and compute all new context unifiers. *)
let add_to_clause
  (config: config)
  (bound: bound)
  (context: context)
  (context_unifier_space: context_unifier_space)
  (input_partner: input_partner)
  (context_partner: context_partner)
  (candidate_type: Context_unifier.candidate_type)
  (close_found: bool)
  : bool =

(*  print_endline ("add_to_clause: " ^ Term.clause_to_string context_unifier_space.Context_unifier.cus_clause);*)

  (* don't compute split unifiers for a lemma *)
  if
    candidate_type == Context_unifier.Split
    &&
    context_unifier_space.Context_unifier.cus_lemma
  then
    close_found

  else begin
    (* which type of search should be done? *)
    let search =
      (* if one close has been found, search only for further closes *)
      if 
	not Const.stable_derivation && close_found
      then
	Context_unifier_search.SearchClose.search_context_unifiers

(*      else if context_unifier_space.Context_unifier.cus_lemma then
	Context_unifier_search.SearchClose.search_context_unifiers*)
      else	  
	match candidate_type with
	  | Context_unifier.Close ->
	      Context_unifier_search.SearchClose.search_context_unifiers
	  | Context_unifier.Assert ->
	      Context_unifier_search.SearchAssert.search_context_unifiers
	  | Context_unifier.Split ->
	      Context_unifier_search.SearchSplit.search_context_unifiers
	  | Context_unifier.CloseAssert ->
	      Context_unifier_search.SearchCloseAssert.search_context_unifiers
	  | Context_unifier.All ->
	      (* no split candidates for lemmas *)
	      if context_unifier_space.Context_unifier.cus_lemma then
		Context_unifier_search.SearchCloseAssert.search_context_unifiers
	      else
		Context_unifier_search.SearchAll.search_context_unifiers
    in

    (* recompute the partial context unifier.
       this might be different for regular clauses and lemmas,
       in which case it might also fail. ??? *)
    let subst =
      Context_unifier.extend_partial_context_unifier
	~recompute:true ~p_preserving:false
	Subst.empty input_partner context_partner
    in
      
    (* finally, compute the new context unifiers / candidates *)
    let close_found' = 
      search
	config
	bound
	context
	context_unifier_space
	input_partner.Context_unifier.ip_index
	context_partner
	subst
    in
      
      (* reorder the clause *)
      begin
	match candidate_type with
	  | Context_unifier.Close
	  | Context_unifier.CloseAssert
	  | Context_unifier.All ->
	      move_updated_input_literal
		context_unifier_space input_partner context_partner config;
	      
	  | Context_unifier.Assert
	  | Context_unifier.Split ->
	      ()
      end;
      
      close_found || close_found'
  end









(*** add ***)



let add_to_problem_literal
  (problem_literals: problem_literals)
  (problem_literal: problem_literal)
  (context_element: Context.element)
  (candidate_type: Context_unifier.candidate_type)
  (close_found: bool)
  subst
  : bool =

  (* literal subsumed? *)
  if is_problem_literal_subsumed_at ~directly:false problem_literals problem_literal context_element then begin
    close_found
  end

(* check each input_partner.
   only the first one is really checked, and the result stored in cache,
   for the others the cached results are just cloned. *)
  else begin
    try
      let close_found =
	ref close_found
      in
	(* cache contains the input partner, context partner, and resolving partner
	   of the first input partner used to compute the partial context unifier
	   with the context partner. *)
      let (_: (input_partner * context_partner * context_partner option) option) =
	Elements.fold
	  (fun _ element cache ->
	     match cache with
		 (* first input_partner, so check it *)
	       | None ->		   
		   (* already resolved? *)
		   if is_input_partner_resolved_at element.input_partner context_element then begin
		     raise Exit
		   end
		     
		   (* try to unify it *)
		   else begin
		     (* does the context literal resolve the clause literal? *)
		     let resolved : context_partner option =
		       resolve problem_literals element.input_partner context_element
		     in

		     (* create the resolving partner, if it exists *)
		     let context_partner =
		       match resolved with
			 | Some partner ->
			     (* reuse the resolving partner *)
			     partner
			       
			 | None ->
			     (* put context literal first,
				as it is then more likely that universal context variables are bound. *)
			     let subst =
			       Subst.replace_offset
				 subst
				 1
				 (Subst.context_literal_offset element.input_partner.Context_unifier.ip_index)
			     in
			       create_context_partner
				 problem_literals element.input_partner context_element subst
		     in
		       
		       (* and add it *)
		       Stack.push element.input_partner.Context_unifier.ip_context_partners context_partner;
		       
		       (* start context unifier computation for all clauses containing the clause literal *)
		       List.iter
			 (fun space ->
			   if is_space_subsumed_at problem_literals space context_element then
			     ()
			       
			   else begin
			     close_found :=
			       add_to_clause problem_literals.config problem_literals.bound
				 problem_literals.context space
				 element.input_partner context_partner candidate_type !close_found
			     end
			 )
			 element.spaces;
		       
		       Some (element.input_partner, context_partner, resolved)
		   end
		     
	       (* literal has already been processed with another index, so reuse its information *)
	     | Some (cached_input_partner, cached_context_partner, cached_resolved) ->

		 (* create the initial context partner *)
		 let new_context_partner =
		   match cached_resolved with
		     | Some _ ->
			 mark_resolved problem_literals element.input_partner context_element

		     | None ->
			 clone_context_partner problem_literals cached_context_partner
			   cached_input_partner element.input_partner
		 in
		   (* and add it *)
		   Stack.push element.input_partner.Context_unifier.ip_context_partners new_context_partner;

		   (* start context unifier computation for all clauses containing the clause literal *)
		   List.iter
		     (fun space ->
			if is_space_subsumed_at problem_literals space context_element then
			  ()

			else begin
			  close_found :=
			    add_to_clause problem_literals.config problem_literals.bound problem_literals.context space
			      element.input_partner new_context_partner candidate_type !close_found
			end
		     )
		     element.spaces;
		   
		   cache
	  )
	  problem_literal.elements
	  None
      in
	!close_found
    with
      | Exit ->
	  close_found
  end



(* add a new context literal and compute all new context unifiers *)
let add (problem_literals: problem_literals)
    (context_element: Context.element) (candidate_type: Context_unifier.candidate_type) : unit =

  match candidate_type with
    | Context_unifier.Assert
    | Context_unifier.Split ->
	failwith "Problem_literals.add"

    | Context_unifier.Close
    | Context_unifier.CloseAssert
    | Context_unifier.All ->
	(* subsume all clauses with the new context literal *)
	subsume problem_literals context_element;
	
	(* unify, i.e. find candidates for partial context unifiers *)
	let unifying_candidates =
	  let index =
	    problem_literals.index#find (Term.request_negated_literal ~insert_db:false context_element.Context.el_literal)
	  in
	    index#find_all_unifiable_subst ~p_preserving:false context_element.Context.el_literal.Term.atom
	in
	  (* try each problem_literal.
	     close_found shows that a closing context unifier has been found. *)
	  ignore (
	    List.fold_left
	      (fun close_found (problem_literal, subst) ->
		 add_to_problem_literal
		   problem_literals
		   problem_literal
		   context_element
		   candidate_type
		   close_found
		   subst
	      )
	      false
	      unifying_candidates
	      : bool)






(*** compute deferred context unifiers ***)



(* computes all context unifiers with the context based on the given candidate_type.
   all partial context unifiers must have been computed previously with function add *)
let compute_deferred' (problem_literals: problem_literals) (candidate_type: Context_unifier.candidate_type)
  : unit =
  (* for each problem literal, i.e. input clause literal ... *)
  problem_literals.index#iter
    (fun _ index ->
      index#iter
	(fun _ problem_literal ->
	  (* ... and for each input literal, i.e. problem literal variant *)
	  Elements.iter
	    (fun _ element ->
	      (* .. check each of its context partners *)
	      Stack.iter
		(fun context_partner ->
		  (* the pseudo-literal v is handled during initialization in any case
		     by context_unifier_space during context_unifier_space creation. *)
		  if
		    Context.element_equal context_partner.Context_unifier.cp_element Context.plus_v_element
		    ||
		    Context.element_equal context_partner.Context_unifier.cp_element Context.minus_v_element
		  then
		    ()
		     
		  (* ignore the clause literal if it is subsumed wrt. to this context literal *)
		  else if is_problem_literal_subsumed_at ~directly:false
		    problem_literals problem_literal context_partner.Context_unifier.cp_element then
		    ()
       
		  (* ignore the clause literal if it is resolved wrt. to this context literal *)
		  else if is_input_partner_resolved_at
		      element.input_partner context_partner.Context_unifier.cp_element then
		    ()
	      
		  (* process every clause containing the clause literal *)
		  else
		    List.iter
		      (fun space ->
			(* ignore if clause is subsumed wrt. to this context literal *)
			if is_space_subsumed_at problem_literals space context_partner.Context_unifier.cp_element then
			  ()
			    
			else begin
			  (* result can be ignored, as a closing context unifier can not be found here. *)
			  ignore (
			    add_to_clause problem_literals.config problem_literals.bound problem_literals.context space
			      element.input_partner context_partner candidate_type false
			      : bool);
			end
		      )
		      element.spaces;
		)
		element.input_partner.Context_unifier.ip_context_partners
	    )
	    problem_literal.elements
	)
    )
    


let compute_deferred (problem_literals: problem_literals) (candidate_type: Context_unifier.candidate_type) : unit =
  match candidate_type with
    | Context_unifier.Close
    | Context_unifier.CloseAssert
    | Context_unifier.All ->
	failwith "Problem_literals.compute_deferred"

    | Context_unifier.Assert
    | Context_unifier.Split ->
	compute_deferred' problem_literals candidate_type






let compute_for_element (problem_literals: problem_literals) (context_element: Context.element)
    (candidate_type: Context_unifier.candidate_type) : unit =
  (* find all potentially unifying problem literals *)
  let unifying_candidates =
    let index =
      problem_literals.index#find (Term.request_negated_literal ~insert_db:false context_element.Context.el_literal)
    in
      index#find_all_unifiable ~p_preserving:false context_element.Context.el_literal.Term.atom
  in

  (* for each problem literal, i.e. input clause literal ... *)
    List.iter
      (fun problem_literal ->
       (* ignore the clause literal if it is subsumed wrt. to this context literal *)
       if is_problem_literal_subsumed_at ~directly:false
	 problem_literals problem_literal context_element then
	   ()
       else
	 (* ... and for each input literal, i.e. problem literal variant *)
	 Elements.iter
	   (fun _ element ->
	      (* ignore the clause literal if it is resolved wrt. to this context literal *)
	      if is_input_partner_resolved_at element.input_partner context_element then
		()

	      else begin
		try
		  (* is there a pair between the context element and this problem literal? *)
		  let context_partner =
		    Stack.find
		      element.input_partner.Context_unifier.ip_context_partners
		      (fun x ->
			 compare
			   context_element.Context.el_id
			   x.Context_unifier.cp_element.Context.el_id
		      )
		  in
		    (* yes, so start context unifier recomputation *)
		    List.iter
		      (fun space ->
			 (* ignore if clause is subsumed wrt. to this context literal *)
			 if is_space_subsumed_at problem_literals space context_partner.Context_unifier.cp_element then
			   ()
						  
			 else begin
			   (* result can be ignored, as a closing context unifier can not be found here. *)
			   ignore (
			     add_to_clause problem_literals.config problem_literals.bound problem_literals.context space
			       element.input_partner context_partner candidate_type false
			       : bool);
			 end
		      )
		      element.spaces;
		with
		  | Not_found ->
		      (* no, so ignore this clause literal *)
		      ()
	      end
	   )
	   problem_literal.elements
      )
      unifying_candidates



(*** backtrack ***)


let backtrack (problem_literals: problem_literals) : unit =
  (* backtrack bindings database *)
  let unneeded_bindings =
    Bindings.fold
      (fun _ (binding, choice_point) acc ->
	 if State.is_choice_point_invalid choice_point then
	   binding :: acc

	 else
	   acc
      )
      problem_literals.bindings
      []
  in
    List.iter
      (Bindings.remove problem_literals.bindings)
      unneeded_bindings;
  

  (* the id of the most recently added context literal *)
  let context_id =
    try
      (Context.most_recent_element problem_literals.context).Context.el_id
    with
      | Not_found ->
	  (* empty context, use -1 for validity comparisions below. *)
	  -1
  in
    (* backtrack clause subsumption *)
    for i = 0 to Array.length problem_literals.subsumed_clauses - 1 do
      if 
	problem_literals.subsumed_clauses.(i) >= 0
	&&
	problem_literals.subsumed_clauses.(i) > context_id
      then
	problem_literals.subsumed_clauses.(i) <- -1;
    done;


    (* backtrack each problem literal *)
    problem_literals.index#iter
      (fun _ index ->
	 index#iter
	   (fun _ problem_literal ->
	 
	     (* backtrack problem literal subsumption *)
	     if
	       problem_literal.subsumed_at >= 0
	       &&
	       problem_literal.subsumed_at > context_id
	     then begin
	       problem_literal.subsumed_at <- -1;
	     end;

	 
	     (* backtrack each input partner *)
	     Elements.iter
               (fun _ element ->
		 (* update resolve state *)
		 begin
		   match element.input_partner.Context_unifier.ip_resolved with
		     | Some context_partner when
			   State.is_choice_point_invalid
			     context_partner.Context_unifier.cp_element.Context.el_choice_point ->
			 Context_unifier.set_resolved element.input_partner None
			   
		     | _ ->
			 ()
		 end;

		 let rec remove_invalid_context_partner () =
		   if Stack.size element.input_partner.Context_unifier.ip_context_partners = 0 then
		     ()

		   else begin
		     let context_partner =
		       Stack.top element.input_partner.Context_unifier.ip_context_partners
		     in
		       if State.is_choice_point_invalid
			 context_partner.Context_unifier.cp_element.Context.el_choice_point
		       then begin
			 Stack.remove_top element.input_partner.Context_unifier.ip_context_partners;
			       
			 begin
			   match context_partner.Context_unifier.cp_partial_context_unifier with
			     | Some subst when
				   Subst.is_empty subst ->
				 (* decrease counter for non-empty substs *)
				 problem_literals.cached_partial_context_unifiers <-
				   problem_literals.cached_partial_context_unifiers - 1;
				     
			     | _ ->
				 ()
				   
			 end;
			   
			 remove_invalid_context_partner ()
		       end

		       else
			 ()
		     end
		 in
		   remove_invalid_context_partner ();
	       )
	       problem_literal.elements;
	   )
      )
