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



type state = State.state
type choice_point = State.choice_point
type context = Context.context
type var = Var.var
type term = Term.term
type literal = Term.literal
type clause = Term.clause
type literal_info = State.literal_info
type subst = Subst.subst

module TermTable = Term.TermTable
module LiteralTable = Term.LiteralTable

(* represents an asserted context literal to regress. *)
type to_regress = {
  (* the context literal to regress. *)
  tr_context_literal: literal;

  (* the id of the context literal in the context. *)
  tr_context_literal_id: int;

  (* the actual instance of the context literal to regress,
     as instantiated and grounded by the current regression. *)
  tr_context_literal_instance: literal;

  (* the dependency information associated with tr_context_literal.
     except for the closing clause this can be retrived via tr_context_literal. *)
  tr_regress_info: literal_info;
}


(* regression is done breadth-first in the order of the context literals,
   starting with the literal most recently added to the context.

   this is achieved by using an ordered set,
   where the max element most recent context literal.

   memoization is achieved implicitely as duplicate elements are ignored in a set. *)
module To_regress =
  Set.Make (
    struct
      type t = to_regress

      let compare (x: to_regress) (y: to_regress) : int =
	(* order by age of context literals *)
	if x.tr_context_literal_id < y.tr_context_literal_id then
	  -1
	else if x.tr_context_literal_id > y.tr_context_literal_id then
	  1
	else
          (* ensure total order by discriminating the same context literal
	     by the instance to regress.
	     this is needed as Set only works properly for total ordered types. *)
	  Term.compare_literals x.tr_context_literal_instance y.tr_context_literal_instance
    end
  )


(* a mapping from a context literal to its regressed instances.
   used to simplify all regressed instances of a context literal. *)
type regressed = (literal list ref) Term.LiteralTable.t




(*** regression ***)




(* add the instance of a context literal to the regressed set *)
let add_regressed (regressed: regressed) (context_literal: literal) (context_literal_instance: literal) : unit =
  let old =
    try
      LiteralTable.find regressed context_literal
    with
      | Not_found ->
	  let old = 
	    ref []
	  in
	    LiteralTable.add regressed context_literal old;
	    old
  in
    old := context_literal_instance :: !old


(* stop regression at some point,
   so do not regress the remaining context literals,
   but instead just keep the corresponding clause literals. *)
let stop_regression (regressed: regressed) (to_regress: To_regress.t) : regressed =
  To_regress.iter
    (fun to_regress ->
       add_regressed
	 regressed
	 to_regress.tr_context_literal
	 (Term.request_negated_literal to_regress.tr_context_literal_instance)
    )
    to_regress;
  regressed






(* compute the unifier that connects the regressed context literal and its paired clause literal,
   and grounds the new context literals to regress. *)
let compute_matching_grounded_unifier current_to_regress =
  (* compute the matching substitution from the clause literal
     to the context literal instantion to regress.
     this might be more special than what is given in the context unifier,
     as now tr_context_literal_instance is used instead of tr_context_literal.
  *)
(*  print_endline ("Ground: " ^ Term.literal_to_string current_to_regress.tr_context_literal_instance);
  print_endline (Subst.subst_to_string current_to_regress.tr_regress_info.State.li_context_unifier);
  print_endline (String.concat " " (List.map Subst.var_to_string current_to_regress.tr_regress_info.State.li_clause_vars));*)

  let matcher =
    (* the closing clause - start of regression *)
    if Term.literal_equal current_to_regress.tr_context_literal Lemma.close_literal then begin
      Subst.empty
    end

    else begin
      let clause_literal =
	List.nth
	  current_to_regress.tr_regress_info.State.li_clause
	  current_to_regress.tr_regress_info.State.li_clause_index
      in
	try
	  Unification.match_terms ~recompute:true
	    clause_literal.Term.atom
	    Subst.input_literal_offset
	    current_to_regress.tr_context_literal_instance.Term.atom
	    (Subst.context_literal_offset current_to_regress.tr_regress_info.State.li_clause_index)
	with
	  | Unification.UNIFICATION_FAIL ->
	      failwith "Lemma.regress_one 1"
    end
  in

  (* now specialize the current context unifier towards the matcher *)
  let context_unifier =
    try
      Unification.unify_substs ~recompute:true
	current_to_regress.tr_regress_info.State.li_context_unifier
	matcher
    with
      | Unification.UNIFICATION_FAIL ->
	  failwith "Lemma_grounded.compute_matching_grounded_unifier 1"
  in

  (* find all variables in bound terms of the unifier,
     and add them to the clause vars.

     clause vars need to be taken into account specially,
     as if a context literal variable was bound to a clause literal variable,
     and then pruned from the resulting unifier to speed up
     merging of partial context unifiers,
     it might be unbound but not in the context unifier. *)
  let bound_vars =
    Subst.fold
      (fun acc binding ->
	 let vars =
	   Term.vars_of_term binding.Subst.sb_term.Subst.st_term
	 in
	   List.fold_left
	     (fun acc var ->
		Tools.list_add
		  Subst.var_equal
		  acc
		  (Subst.make_var var binding.Subst.sb_term.Subst.st_offset)
	     )
	     acc
	     vars
      )
      current_to_regress.tr_regress_info.State.li_clause_vars
      context_unifier
  in

    (* ground all unbound variables. *)
    List.fold_left
      (fun grounded_context_unifier var ->
	 match Subst.get grounded_context_unifier var with
	   | None ->
	       (* unbound -> ground *)
	       let term =
		 Term.request_const (Symbol.create_skolem ())
	       in
	       let grounded_context_unifier' =
		 try
		   Subst.set ~recompute:true
		     grounded_context_unifier
		     var
		     (Subst.make_term term Subst.input_literal_offset) (* offset doesn't matter here *)
		 with
		   | Subst.SET_FAIL ->
		       failwith "Lemma_grounded.compute_matching_grounded_unifier 2"
	       in
		 grounded_context_unifier'
		   
	   | _ ->
	       (* bound -> ignore *)
	       grounded_context_unifier
      )
      context_unifier
      bound_vars


(* regress one context literal *)
let regress_one
    (state: state)
    (context: context)

    (* only regress context literals up to this choice point *)
    (regress_stop: choice_point)
    (* already regressed context literals *)
    (regressed: regressed)
    (* context literals to regress *)
    (to_regress: To_regress.t)
    (* the curent context literal to regress *)
    (current_to_regress: to_regress)
    :
    (* the extended regressed set *)
    regressed
    *
    (* the extended to regress set *)
    To_regress.t
    =
(*  print_endline ("regress: " ^ Term.literal_to_string current_to_regress.tr_context_literal);
  print_endline ("  ---->: " ^ Term.literal_to_string current_to_regress.tr_context_literal_instance);
  print_endline ("  ---->: " ^ Term.clause_to_string current_to_regress.tr_regress_info.State.li_clause);*)
  let grounded_context_unifier =
    compute_matching_grounded_unifier current_to_regress
  in

  (* check for each context literal in the context unifier to regress how to regress it. *)
  let rec regress_one' (index: int) (regressed: regressed) (to_regress: To_regress.t) :
      regressed * To_regress.t
      =

    (* all context literals done *)
    if index >= Array.length current_to_regress.tr_regress_info.State.li_context_literals then begin
      regressed, to_regress
    end
    
    (* the regressed assert literal - just ignore it.
       this does not cover a a unit split,
       in that case the context literal instantiated to the split literal is regressed. *)
    else if (Term.literal_equal current_to_regress.tr_regress_info.State.li_context_literals.(index) Term.assert_literal) then begin
    (*else if index == current_to_regress.tr_regress_info.State.li_clause_index then begin*)
      regress_one' (index + 1) regressed to_regress
    end

    (* process this context_literal *)
    else begin
      let clause_literal =
	List.nth current_to_regress.tr_regress_info.State.li_clause index
      in
      let clause_literal_instance =
	Subst.apply_to_literal
	  grounded_context_unifier
	  clause_literal
	  Subst.input_literal_offset
      in
      let context_literal =
	current_to_regress.tr_regress_info.State.li_context_literals.(index)
      in
	if Const.debug && not (Term.is_literal_ground clause_literal_instance) then begin
	  print_endline (Term.literal_to_string clause_literal_instance);
	  failwith "Lemma_grounded.regress_one: clause_literal_instance not ground";
	end;

      	(* check type of context literal *)

	(* the default pseudo-literal -v - no regression, just add the clause literal *)
	if
	  Term.literal_equal context_literal Term.minus_v
	  ||
	  Term.literal_equal context_literal Term.plus_v
	then begin
	  add_regressed regressed context_literal clause_literal_instance;
	  regress_one' (index + 1) regressed to_regress
	end
	  

	else begin
	  let literal_info' =
	    try
	      State.get_literal_info state context_literal
	    with
	      | Not_found ->
		  failwith ("Lemma.regress_one: context literal " ^ Term.literal_to_string context_literal)
	  in
	  let explanation =
	    State.get_explanation state [| context_literal |]
	  in
	    match explanation with
	      | [] ->
		  (* an assert at the root choice point -
		     entailed by the input, just ignore it *)
(*		  if
		    (State.choice_point_equal literal_info'.State.li_choice_point (State.root_choice_point state))
		    &&
		    (* unless this is based on an assumption that will be retracted later on,
		       e.g. the domain size. *)
		    (not (Term.is_fd_size_marker context_literal))
		  then
		    regress_one' (index + 1) regressed to_regress

		  (* a right split, learn it *)
		  else*) begin
		    add_regressed regressed context_literal clause_literal_instance;
		    regress_one' (index + 1) regressed to_regress
		  end
		      
	      | creation_point :: _ when
		  (State.compare_age regress_stop creation_point > 0) ->
		  (* an assert older than the choice points within regression is done -
		     no regression, just add the clause literal *)
		  add_regressed regressed context_literal clause_literal_instance;
		  regress_one' (index + 1) regressed to_regress
		    
	      | _ ->
		  (* a Split - no regression, just add the clause literal.
		     this does not catch Unit Splits, as those have type Assert. *)
		  if
		    literal_info'.State.li_type = State.SplitLeft
		    ||
		    literal_info'.State.li_type = State.SplitRight
		  then begin
		    add_regressed regressed context_literal clause_literal_instance;
		    regress_one' (index + 1) regressed to_regress
		  end
		    
		  (* the normal case, an actual assert literal to regress *)
		  else begin
		    let context_id =
		      match Context.element_for_literal context context_literal with
			| Some element -> element.Context.el_id
			    
			| None -> failwith "Lemma_grounded: context_id"
		    in
		    let context_literal_instance =
		      Term.request_negated_literal clause_literal_instance
		    in
(*  print_endline ("regress add: " ^ Term.literal_to_string context_literal);
  print_endline ("  ---->: " ^ Term.literal_to_string context_literal_instance);
  print_endline ("  ---->: " ^ Term.clause_to_string literal_info'.State.li_clause);*)
		    let new_to_regress = {
		      tr_context_literal = context_literal;
		      tr_context_literal_id = context_id;
		      tr_context_literal_instance = context_literal_instance;
		      tr_regress_info = literal_info';
		    }
		    in
		      regress_one' (index + 1)
			regressed
			(To_regress.add new_to_regress to_regress)
		  end
	end
    end
  in
    regress_one' 0 regressed to_regress



(* uip reached? *)
let uip_reached (to_regress: To_regress.t) : bool =
  let first =
    To_regress.max_elt to_regress
  and last =
    To_regress.min_elt to_regress
  in
    (* only one context literal left to regress. *)
    (Term.literal_equal first.tr_context_literal last.tr_context_literal)
    &&
    (* ignore the closing clause. *)
    (not (Term.literal_equal first.tr_context_literal Lemma.close_literal))


(* regress all context literals to regress *)
let rec regress' ~(uip: bool)
    (state: state)
    (context: context)
    (* only regress context literals up to this choice point *)
    (regress_stop: choice_point)
    (* clause literal * offset contained in the regressed literal set *)
    (regressed: regressed)
    (* branch nodes to regress * offset *)
    (to_regress: To_regress.t)
    :
    (* the regressed set *)
    regressed
    =

  (* nothing more to regress *)
  if To_regress.is_empty to_regress then begin
    regressed
  end

  (* if we reached the UIP, stop right here *)
  else if uip && uip_reached to_regress then begin
    stop_regression regressed to_regress
  end
  
  (* otherwise regress most recent context literal instance *)
  else begin    
    let next =
      To_regress.max_elt to_regress
    in
    let tail =
      To_regress.remove next to_regress
    in
    let regressed', to_regress' =
      regress_one state context regress_stop regressed tail next
    in
      regress' ~uip:uip state context regress_stop regressed' to_regress'
  end




(* wrapper for regress'. *)
let regress ~(uip: bool)
    (state: state)
    (context: context)
    (* only regress context literals up to this choice point *)
    (regress_stop: choice_point)
    (* clause literal * offset contained in the regressed literal set *)
    (to_regress: To_regress.t)
    :
    (* the regresset set *)
    regressed
    =
  regress' ~uip:uip
    state context regress_stop
    (LiteralTable.create 251)
    to_regress






(*** simplification ***)

(* replace each skolem constant by a fresh variable.
   returns a universal normalized literal list (or clause). *)
let generalize (regressed: regressed) : literal list list =
  let var_id =
    ref (-1)
  in
  let mapping =
    TermTable.create 64
  in

  (* map all skolem constants to a fresh variable *)
  let rec find_skolem_constants term =
    match term with
      | Term.Var _ ->
          ()
	  
      | Term.Const symbol ->
          if
            Symbol.is_skolem symbol
            &&
            not (TermTable.mem mapping term)
          then begin
            var_id := !var_id + 1;
            TermTable.add mapping term (Term.request_var (Var.create_universal !var_id))
          end
		   
      | Term.Func func ->
          Array.iter find_skolem_constants func.Term.subterms
  in
    LiteralTable.iter
      (fun _ literals ->
         List.iter
	 (fun literal ->
	    find_skolem_constants literal.Term.atom
	 )
	 !literals
      )
      regressed;
    
    (* apply the mapping to the literals *)
    let mapping' =
      TermTable.fold
        (fun skolem var acc ->
           (skolem, var) :: acc
        )
        mapping
        []
    in
      
      LiteralTable.fold
        (fun _ literals acc ->
	   let literals' =
	     List.map
	       (fun literal ->
                  Term.replace_terms_in_literal literal mapping'
	       )
	       !literals
	   in
	     literals' :: acc
        )
        regressed
	[]






      

(*** main function ***)


let get_lemma ~(uip: bool) (state: state) (context: context) (context_unifier: subst)
  (clause: clause) (conflict_literals: literal array) (regress_stop: choice_point) : clause option =
  (* fake data to get the regression with the closing clause started *)
(*  print_newline ();
  print_newline ();
  print_endline ("Closing Clause: " ^ Term.clause_to_string clause);*)

  let closing_info =
    State.create_literal_info Lemma.close_literal State.Propagation
      (State.active_choice_point state) context_unifier
      clause []
      (List.map (fun var -> Subst.make_var var Subst.input_literal_offset) (Term.vars_of_clause clause))
      conflict_literals (-1) (fun () -> ())
      (* must just have some dependency to not be taken as a root assert *)
      (Some [regress_stop])
  in
  let regression_start = {
    tr_context_literal = Lemma.close_literal;
    (* pseudo-context literal: most recent context element *)
    tr_context_literal_id = Pervasives.max_int;
    tr_context_literal_instance = Lemma.close_literal;
    tr_regress_info = closing_info;
  }
  in

  (* do the regression *)
  let regressed =
    regress ~uip:uip
      state context regress_stop
      (To_regress.add regression_start To_regress.empty)
  in

  let unground =
    generalize regressed
  in
  let simplified =
    Term.remove_duplicates (Lemma.simplify unground)
  in
  let normalized =
    Term.sort_clause simplified
  in
    (* needed to avoid learning right splits as real lemmas *)
    match clause, normalized with
      | unit_clause :: [], unit_lemma :: [] when
	  Unification.is_literal_instance unit_lemma unit_clause ->
	  None
	    
      | _ ->
	  (* normalize variables. *)
	  Some (Subst.apply_to_clause Subst.empty normalized 0)
