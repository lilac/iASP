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

type var = Var.var
type literal = Term.literal
type clause = Term.clause
type subst = Subst.subst
type choice_point = State.choice_point
type state = State.state
type 'a stack = 'a Stack.stack

type candidate_type =
  | Close
  | Assert
  | Split
  | CloseAssert
  | All

type context_partner = {
  cp_element: Context.element;
  cp_partial_context_unifier: subst option;
  cp_empty_remainder: bool;
}

type context_partners =
    context_partner array

type input_partner = {
  ip_index: int;
  ip_literal: literal;
  ip_vars: Subst.var list;
  mutable ip_context_partners: context_partner stack;
  mutable ip_resolved: context_partner option;
}

type context_unifier_space = {
  cus_id: int;
  cus_clause: clause;
  cus_lemma: bool;
  mutable cus_lemma_learned: int;
  cus_vars: Subst.var list;
  cus_shared_vars: Subst.var list;
  cus_local_vars: Subst.var list;
  cus_input_partners: input_partner array;
  cus_input_partners_ordering: int array;
  cus_process_close_candidate: (raw_context_unifier -> unit);
  cus_process_assert_candidate: (raw_context_unifier -> bool);
  cus_process_split_candidate: (raw_context_unifier -> unit);
  cus_is_element_incomplete_assert: Context.element -> bool;
  cus_is_element_incomplete_split: Context.element -> bool;
  cus_utility: int ref;
  cus_constraints: Finite_domain_constraints.constraints option;
}


and raw_context_unifier = {
  rcu_space: context_unifier_space;
  rcu_context_partners: context_partners;
  rcu_exceeding_complexity: bool;
  rcu_constraints: Finite_domain_constraints.solution option;
}




exception UNSATISFIABLE
exception CLOSE of raw_context_unifier



(*** constants ***)


let assert_partner = {
  cp_element = Context.assert_element;
  cp_partial_context_unifier = Some Subst.empty;
  cp_empty_remainder = true;
}

let null_partner = {
  cp_element = Context.null_element;
  cp_partial_context_unifier = Some Subst.empty;
  cp_empty_remainder = true;
}

let null_space = {
  cus_id = -1;
  cus_clause = [];
  cus_vars = [];
  cus_lemma = false;
  cus_lemma_learned = 0;
  cus_shared_vars = [];
  cus_local_vars = [];
  cus_input_partners = [| |];
  cus_input_partners_ordering = [| |];
  cus_process_close_candidate = (fun _raw_context_unifier -> ());
  cus_process_assert_candidate = (fun _raw_context_unifier -> false);
  cus_process_split_candidate = (fun _raw_context_unifier -> ());
  cus_is_element_incomplete_assert = (fun _ -> false);
  cus_is_element_incomplete_split = (fun _ -> false);
  cus_utility = ref 0;
  cus_constraints = None;
}

let null_context_unifier = {
  rcu_space = null_space;
  rcu_context_partners = [| |];
  rcu_exceeding_complexity = false;
  rcu_constraints = None;
}


(*** misc ***)



let get_input_clause (context_unifier_space: context_unifier_space) : clause =
  match context_unifier_space.cus_constraints with
    | None ->
	context_unifier_space.cus_clause
    | Some constraints ->
	Finite_domain_constraints.get_clause constraints
	


let get_context_literals (context_partners: context_partners) : literal array =
  Array.map
    (fun context_literal ->
       context_literal.cp_element.Context.el_literal
    )
    context_partners




(*** representation ***)


let context_unifier_to_string
  (context_unifier_space: context_unifier_space) (context_partners: context_partners)
  : string =
  let context_literals =
    Array.to_list (
      Array.map
	(fun context_partner ->
(*	   string_of_int context_partner.cp_element.Context.el_id
	   ^ ": "
	   ^*) Term.literal_to_string context_partner.cp_element.Context.el_literal
	)
	context_partners
    )
  in
  (
    if context_unifier_space.cus_lemma then
      "Lemma : "
    else
      "Clause : "
  )
  (*^ string_of_int context_unifier_space.cus_id ^ ": "*)
    ^ Term.clause_to_string (get_input_clause context_unifier_space)
    ^ "\n"
    ^ "Context: "
    ^ "["
    ^ String.concat ", " context_literals ^ "]"
(*  ^ Term.clause_to_string
    (
      Array.to_list (get_context_literals context_partners)) *)
    
  ^ "\n"


let raw_context_unifier_to_string (raw_context_unifier: raw_context_unifier) : string =
  context_unifier_to_string
    raw_context_unifier.rcu_space
    raw_context_unifier.rcu_context_partners


















(*** misc ***)


(* as long as the choice points of all its context literals are valid,
   the context unifier is valid *)
let is_raw_context_unifier_invalid (raw_context_unifier: raw_context_unifier) : bool =
  Tools.array_exists
    (fun context_partner ->
       State.is_choice_point_invalid context_partner.cp_element.Context.el_choice_point
    )
    raw_context_unifier.rcu_context_partners




let extend_partial_context_unifier ~(recompute: bool) ~(p_preserving: bool)
    (subst: subst) (input_partner: input_partner) (context_partner: context_partner) : subst =
  match context_partner.cp_partial_context_unifier with
    | Some partial_context_unifier ->
	(* cheap pre-check *)
(*	if not recompute then
          Unification.unify_substs_shallow subst partial_context_unifier;*)

        Unification.unify_substs
	  ~recompute:recompute
	  ~p_preserving:p_preserving
	  subst
	  partial_context_unifier
	  
    | None ->
	(* special case: pseudo literal v *)
	if
	  Context.element_equal context_partner.cp_element Context.plus_v_element
	  ||
	  Context.element_equal context_partner.cp_element Context.minus_v_element
	then begin
	  failwith "extend_partial_context_unifier";
	  (*
	  Subst.set_ ~p_preserving:false ~i_preserving:false
	    subst
	    Term.v_par
	    (Subst.context_literal_offset input_partner.ip_index)
	    Term.v_term
	    (Subst.context_literal_offset input_partner.ip_index)
	  *)
	end
	    
	(* special case: ignore assert gap -
	   automatically done as the constant context_partner
	   has a cached partial context unifier
	else if Context.is_equal context_partner.cp_element Context.assert_element then
	  subst
	*)

	else begin	  
	  (* put context literal first,
	     as it is then more likely than universal context variables are bound. *)
	  let subst =
	    Unification.unify_terms_
	      ~recompute:recompute
	      ~p_preserving:p_preserving
	      subst
	      context_partner.cp_element.Context.el_literal.Term.atom
	      (Subst.context_literal_offset input_partner.ip_index)
	      input_partner.ip_literal.Term.atom
	      Subst.input_literal_offset
	  in
	    subst
	end

let recompute_unifier ~(recompute: bool) (raw_context_unifier: raw_context_unifier) : subst =

  let rec recompute_at (i: int) (acc: subst) : subst =
    if i >= Array.length raw_context_unifier.rcu_space.cus_input_partners then
      acc

    else
      let input_partner =
	raw_context_unifier.rcu_space.cus_input_partners.(i)
      in
      let input_index =
	input_partner.ip_index
      in
      let context_partner =
	raw_context_unifier.rcu_context_partners.(input_index)
      in
      let extended_unifier =
	try
	  extend_partial_context_unifier ~recompute:recompute ~p_preserving:false
	    acc input_partner context_partner
	with
	  | Unification.UNIFICATION_FAIL ->
	      print_endline (Subst.subst_to_string acc);
	      print_endline (Term.literal_to_string input_partner.ip_literal);
	      print_endline (Term.literal_to_string context_partner.cp_element.Context.el_literal);
	      print_endline (raw_context_unifier_to_string raw_context_unifier);
	      failwith "Context_unifier.recompute_unifier"
      in
	recompute_at (i + 1) extended_unifier
	  
  in
  let subst =
    match raw_context_unifier.rcu_constraints, raw_context_unifier.rcu_space.cus_constraints with
      | None, None -> Subst.empty
      | Some solution, Some constraints ->
	  (Finite_domain_constraints.base_subst constraints)
	  @
	  (Finite_domain_constraints.solution_to_subst solution)
      | _, Some constraints ->
	  print_endline (raw_context_unifier_to_string raw_context_unifier);
	  print_endline (Term.clause_to_string (Finite_domain_constraints.get_unconstrained constraints));
	  print_endline (Term.clause_to_string (Finite_domain_constraints.get_constrained constraints));
	  print_endline (Subst.subst_to_string (Finite_domain_constraints.base_subst constraints));
	  failwith "Context_unifier.recompute_unifier: constraint1"
      | _ ->
(*      | _ ->*)
	  failwith "Context_unifier.recompute_unifier: constraint"
  in
    recompute_at 0 subst



let recompute_full_unifier ~(recompute: bool) (raw_context_unifier: raw_context_unifier) : subst =
  recompute_unifier ~recompute:recompute raw_context_unifier



(* is this pair producing a remainder literal?
   yes, if one of its parameters is bound to a non-parameter *)
let is_remainder (subst: subst) (context_element: Context.element) (offset: int) =
  let par_offset =
    Subst.context_literal_offset offset
  in
(*  Subst.exists
    (fun binding ->
       binding.Subst.sb_var.Subst.sv_offset == par_offset
       &&
       Var.is_parametric binding.Subst.sb_var.Subst.sv_var
       &&
       match binding.Subst.sb_term.Subst.st_term with
	 | Term.Var var when Var.is_parametric var ->
	     false

	 | _ ->
	     true
    )
    subst
*)
    List.exists
      (fun par ->
	 match Subst.get' subst par par_offset with
	   | None -> 
	       false
	       
	   | Some term ->
	       begin
		 match term.Subst.st_term with
		   | Term.Var var ->
		       Var.is_universal var

		   | _ -> 
		       true
	       end
      )
      context_element.Context.el_pars

let is_remainder' (subst: subst)  =
  Subst.exists
    (fun binding ->
       Var.is_parametric binding.Subst.sb_var.Subst.sv_var
       &&
       match binding.Subst.sb_term.Subst.st_term with
	 | Term.Var var when Var.is_parametric var ->
	     false

	 | _ ->
	     true
    )
    subst




let compute_remainder_states (raw_context_unifier: raw_context_unifier) (subst: subst) : bool array =
  (* no map function as the arrays have a different order *)
  let remainder_states =
    Array.make (Array.length raw_context_unifier.rcu_context_partners) false
  in

    for i = 0 to Array.length raw_context_unifier.rcu_space.cus_input_partners - 1 do
      let input_index =
	raw_context_unifier.rcu_space.cus_input_partners.(i).ip_index
      in
      let context_partner =
	raw_context_unifier.rcu_context_partners.(input_index)
      in
	remainder_states.(input_index) <-
	  not context_partner.cp_empty_remainder
	  ||
	  is_remainder
	  subst
	  context_partner.cp_element
	  input_index
    done;

    remainder_states




(* a generation of a candidate is
   the max generation of its context_partners + 1 *)
let generation_of_candidate (context_partners: context_partners) : int =
  let max_generation =
    Array.fold_left
      (fun acc context_partner ->
	 Tools.max_int acc context_partner.cp_element.Context.el_generation
      )
      0
      context_partners
  in
    max_generation + 1


let get_creation_choice_point (context_partners: context_partners) : choice_point =
  let youngest_choice_point =
    Array.fold_left
      (fun acc context_partner ->
	 match acc with
	   | Some best_choice_point when
	       State.compare_age best_choice_point context_partner.cp_element.Context.el_choice_point >= 0 ->
	       acc

	   | _ ->
	       Some context_partner.cp_element.Context.el_choice_point
      )
      None
      context_partners
  in
    match youngest_choice_point with
      | None ->
	  failwith "Context_unifier.get_creation_choice_point"

      | Some choice_point ->
	  choice_point



let creating_context_element (raw_context_unifier: raw_context_unifier) : Context.element =
  let most_recent =
    Array.fold_left
      (fun acc context_partner ->
	 match acc with
	   | Some best when
	       compare best.Context.el_id context_partner.cp_element.Context.el_id >= 0 ->
	       acc

	   | _ ->
	       Some context_partner.cp_element
      )
      None
      raw_context_unifier.rcu_context_partners
  in
    match most_recent with
      | None ->
	  failwith "Context_unifier.creating_context_element"

      | Some element ->
	  element




let is_permanently_blocking (blocking: Context.element) (blocked: context_partners) : bool =
  Tools.array_exists
    (fun partner ->
       State.backtracking_depends_on
	 partner.cp_element.Context.el_choice_point
	 blocking.Context.el_choice_point
    )
    blocked


let create_space_utility_inc (space: context_unifier_space) : (unit -> unit) =
  let f () =
    space.cus_utility := !(space.cus_utility) + 1
  in
    f





let compare_context_unifiers ~(different: bool) (first: raw_context_unifier) (second: raw_context_unifier) : int =

  (* as first and second are context unifiers of the same clause
     they must have the same number of context partners *)
  let rec cmp_context_partners (index: int) =
    if index >= Array.length first.rcu_context_partners then begin
      (* constrained part differs? *)
      match first.rcu_constraints, second.rcu_constraints with
	| None, None ->
	    (* there can only be one valid candidate with at a position in the ordering *)
	    if
	      different
	      &&
	      not (is_raw_context_unifier_invalid first)
	      &&
	      not (is_raw_context_unifier_invalid second)
	    then begin
	      print_endline (raw_context_unifier_to_string first);
	      print_endline (raw_context_unifier_to_string second);
	      failwith "Context_unifier.compare_context_unifiers";
	    end

	    else begin
	      (* at least one of the candidates is invalid *)
	      0
	    end

	| Some solution, Some solution' ->
	    let subst_cmp =
	      Finite_domain_constraints.solution_compare solution solution'
	    in
	      if subst_cmp != 0 then
		subst_cmp
	      else if
		different
		&&
		not (is_raw_context_unifier_invalid first)
		&&
	      not (is_raw_context_unifier_invalid second)
	      then begin
		print_endline (raw_context_unifier_to_string first);
		print_endline (raw_context_unifier_to_string second);
		failwith "Context_unifier.compare_context_unifiers 2";
	      end
		
 	      else
		0

	| _ ->
	    failwith "Context_unifier.compare_context_unifiers 3";
    end
    else
      let cmp =
	compare
	  first.rcu_context_partners.(index).cp_element.Context.el_id
	  second.rcu_context_partners.(index).cp_element.Context.el_id
      in
	if cmp != 0 then
	  cmp
	else
	  cmp_context_partners (index + 1)
  in

    if first == second then
      0
    else
      let cmp_creation_point =
	Tools.compare_int
          (creating_context_element first).Context.el_id
          (creating_context_element second).Context.el_id
      in
	if cmp_creation_point <> 0 then
          cmp_creation_point
	else
	  let cmp_clause =
	    compare first.rcu_space.cus_id second.rcu_space.cus_id
	  in
	    if cmp_clause <> 0 then
	      cmp_clause
	    else if
	      Const.debug
	      &&
		(Array.length first.rcu_context_partners != Array.length second.rcu_context_partners)
	    then
	      failwith "Context_unifier.cmp_context_partners 1"
	    else
	      cmp_context_partners 0


let create_context_unifier space partners complexity constraints =
  if Const.debug then begin
    match constraints, space.cus_constraints with
      | None, Some _
      | Some _, None ->
	  failwith "Context_unifier.create_context_unifier"
      | _ -> ()
  end;
  {
    rcu_space = space;
    rcu_context_partners = partners;
    rcu_exceeding_complexity = complexity;
    rcu_constraints = constraints;
  }

let create_context_partner element subst empty_remainder =
  {
    cp_element = element;
    cp_partial_context_unifier = subst;
    cp_empty_remainder = empty_remainder;
  }

let create_input_partner index literal vars =
  {
    ip_index = index;
    ip_literal = literal;
    ip_vars = vars;
    ip_context_partners = Stack.create null_partner;
    ip_resolved = None;
  }



(*** globals ***)



let create_space id clause clause_vars shared_vars local_vars input_partners ordering
    process_close process_assert process_split
    is_element_incomplete_assert is_element_incomplete_split is_lemma constraints =
  {
    cus_id = id;
    cus_clause = clause;
    cus_vars = clause_vars;
    cus_shared_vars = shared_vars;
    cus_local_vars = local_vars;
    cus_input_partners = input_partners;
    cus_input_partners_ordering = ordering;
    cus_process_close_candidate = process_close;
    cus_process_assert_candidate = process_assert;
    cus_process_split_candidate = process_split;
    cus_is_element_incomplete_assert = is_element_incomplete_assert;
    cus_is_element_incomplete_split = is_element_incomplete_split;
    cus_lemma = is_lemma;
    cus_lemma_learned = if is_lemma then 1 else 0;
    cus_utility = ref 0;
    cus_constraints = constraints;
  }


let set_resolved input_partner resolved =
  input_partner.ip_resolved <- resolved

let increase_lemma_learned space =
  space.cus_lemma_learned <- space.cus_lemma_learned + 1

let get_constrained_clause context_unifier =
  match context_unifier.rcu_space.cus_constraints with
    | None ->
	[]
    | Some constraints ->
	Finite_domain_constraints.get_constrained constraints
