(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2006
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


let debug_print = false



(* regression algorithm

   the regression alogrithm computes
   - a set of constraints
   - a set of regressed literals

   a constraint is a pairing of two literals with their offsets
   that has to be unified, e.g. (f(x), 0), (f(a), 1).
   after the regression all constraints are unified to get one substitution,
   e.g. the above constraint yields [ 0:x -> 1:a ].

   the regressed literals are the leaves of the regression tree,
   i.e. pairs of literals and offsets, e.g. (g(x, b), 0).

   the regression (conflict set) consists of the application
   of the constraint unifier to the regressed literals,
   in this example { g(a, b) }.


   the actual regression is started from the closing clause,
   which is put paired with a fresh offset into the to_regress list.
   the fresh offset in effect creates a fresh variant of each clause
   used in a context unifier and the regression.
   the constraints and regressed literals are successively built
   buy regressing each clause in the to_regress list,
   which in turn might add new clauses to the regress list.

   a single clause is regressed by regressing each clause literal,
   or more precisely the context literal paired with the clause literal
   in the corresponding context unifier. That is, for the closing clause
   the context literals of the closing context unifier,
   and then in turn the context unifiers of the regressed context literals.
   
   now, there are several possibilities wrt. to regressing a clause literal L
   resp. context literal K:
   
   - Split
   if the context literal K was created by a (left or right) split decision,
   it is not regressed, as it is not implied, but simply guessed.
   and the clause literal L and the clause offset are taken as the regressed literal.
   
   - Assert
   if the context literal K was created by an assert,
   the context unifier creating K is added to the to_regress list with a fresh offset.
   Furthermore, a new constraint between the clause literal L
   and the clause literal L' which was instantiated to K in K's context unifier,
   i.e. the clause literal that was paired with no clause literal in the assert context unifier,
   is added.
   L is paired with its clause context unifier, L' with its fresh offset.

   Thus, the assert literal is replaced by the context literals used to compute it,
   and its instantiation effect is caught in the new constraint.

   - Root Choice Point, -v or +v
   These can not be regressed and are treated as Splits


   Furthermore, there are some limits which may stop the regression early.
   Most importantly, only the assert context literals of the decision level
   in which the conflict occurred are regresed,
   others are also simply treated as Splits.

*)



(*** types ***)



type state = State.state
type context = Context.context
type choice_point = State.choice_point
type var = Var.var
type term = Term.term
type literal = Term.literal
type clause = Term.clause
type literal_info = State.literal_info
type subst = Subst.subst


module LiteralTable = Term.LiteralTable

(* represents an asserted context literal to regress. *)
type to_regress = {
  (* the context literal to regress. *)
  tr_context_literal: literal;

  (* the id of the context literal in the context. *)
  tr_context_literal_id: int;

  (* the clause literal paired with the context literal in the clause to regress. *)
  tr_clause_literal: literal;
 
  (* the dependency information associated with tr_context_literal.
     except for the closing clause this can be retrived via tr_context_literal. *)
  tr_regress_info: literal_info;
}


(* regression is done breadth-first in the order of the context literals,
   starting with the literal most recently added to the context.

   this is achieved by using an ordered set,
   where the max element most recent context literal.

   memoization is achieved explicitely by regressing the min element first. *)
module To_regress =
  Set.Make (
    struct
      type t = to_regress

      let compare (x: to_regress) (y: to_regress) : int =
	Tools.compare_int x.tr_context_literal_id y.tr_context_literal_id
    end
  )


(* a mapping from a context literal to its regressed clause literals.
   used to simplify all regressed instances of a context literal.

   variants are created via the int offset. *)
type regressed = {
  (* regressed literals *)
  regressed_literals: ((literal * int) list ref) LiteralTable.t;
  (* number of regressed literals *)
  mutable regressed_size: int;
}


(* a memoized regression of a context literal.

   the offsets used to create fresh variants range from 0 .. mm_max_offset.

   if another regression uses this memoization,
   it needs to connect via a constraint to (mm_clause_literal, 0).
*)
type memoized = {
  (* the clause literal the regressed context literal is an instance of.
     has implicitely offset 0. *)
  mm_clause_literal: literal;

  (* max. used offset in regressed and constraints *)
  mm_max_offset: int;

  (* regressed literals *)
  mm_regressed: regressed;

  (* regressed ground clause literals
     which are instances of ground context literals. *)
  mm_ground: literal list;

  (* the contraints. *)
  mm_constraints: ((literal * int) * (literal * int)) list;

  (* if mm_clause_literal is ground,
     then any regression using this memoization will have no connection
     to the variables in mm_regressed and mm_constraints by the unifier.

     thus, every time this memoization is used,
     it will just lead to the creation of a variant,
     and evtl. all these variants can be condensed to one.
     
     thus, it sufficed to just return it once.

     if mm_disconnected = true,
     then this has happened,
     i.e. mm_clause_literal is ground and this memoization has been used. *)
  mutable mm_disconnected: bool;
}

(* memoization table for a context literal *)
module MemoizationTable = Term.LiteralTable
type memoization = memoized MemoizationTable.t


(* raised if size of regressed literals > Const.lemma_max_constraints *)
exception LIMIT

(* used to group finite domain literals during the regression
   in a regressed data structure.
   in constrained mode these literals are unpaired,
   so this seems to be the simplest way to handle this. *)
let regressed_fd_literal =
  Term.request_literal
    true
    (Term.request_const
       (Symbol.create_skolem
	  ~name:(Some "__lemma_regressed_fd_literal")
	  ()
       )
    )


let rec find_uip
  (state: state)
  (context: context)
  (regress_stop: choice_point)
  (to_regress: To_regress.t)
  : int
  =
  if To_regress.is_empty to_regress then begin
    failwith "Lemma_lifted.find_uip: empty regression";
  end;

  let first =
    To_regress.max_elt to_regress
  and last =
    To_regress.min_elt to_regress
  in
    (* UIP reached? *)
    if
      (* only one context literal left to regress. *)
      (Term.literal_equal first.tr_context_literal last.tr_context_literal)
      &&
      (* ignore the closing clause. *)
      (not (Term.literal_equal first.tr_clause_literal Lemma.close_literal))
    then begin
      first.tr_context_literal_id
    end

    (* don't regress  a right split. *)
    else if first.tr_regress_info.State.li_type = State.SplitRight  then begin
      find_uip state context regress_stop (To_regress.remove first to_regress)
    end

    (* regress the dependencies *)
    else begin
      let current_to_regress =
	first
      in
      let to_regress =
	To_regress.remove current_to_regress to_regress
      in

      let rec find_uip' (index: int) (to_regress: To_regress.t) : To_regress.t =
	(* all context literals done *)
	if index >= Array.length current_to_regress.tr_regress_info.State.li_context_literals then begin
	  to_regress
	end
    
	(* the regressed assert literal - just ignore it.
	   this does not cover a a unit split,
	   in that case the context literal instantiated to the split literal is regressed. *)
	else if (Term.literal_equal current_to_regress.tr_regress_info.State.li_context_literals.(index) Term.assert_literal) then begin
	  (*else if index == current_to_regress.tr_regress_info.State.li_clause_index then begin*)
	  find_uip' (index + 1) to_regress
	end

	(* process this context_literal *)
	else begin
	  let clause_literal =
	    List.nth current_to_regress.tr_regress_info.State.li_clause index
	  in
	  let context_literal =
	    current_to_regress.tr_regress_info.State.li_context_literals.(index)
	  in

	    (* ignore -v *)
	    if
	      Term.literal_equal context_literal Term.minus_v
	      ||
	      Term.literal_equal context_literal Term.plus_v
	    then begin
	      find_uip' (index + 1) to_regress
	    end
	      
	    else begin
	      let explanation =
		State.get_explanation state [| context_literal |]
	      in
		match explanation with
		  | [] ->
		      (* ignore root assert *)
		      find_uip' (index + 1) to_regress
		      
		  | creation_point :: _ when
		      (State.compare_age regress_stop creation_point > 0) ->
		      (* ignore assert older than the choice points to regress. *)
		      find_uip' (index + 1) to_regress

		  | _ ->
		      (* add all Split and Assert literals to regress. *)
		      let literal_info' =
			try
			  State.get_literal_info state context_literal
			with
			  | Not_found ->
			      failwith ("Lemma_lifted.find_uip: context literal " ^ Term.literal_to_string context_literal)
		      in
		      let context_id =
			match Context.element_for_literal context context_literal with
			  | Some element -> element.Context.el_id
			      
			  | None -> failwith "Lemma_lifted.find_uip': context_id"
		      in
		      let new_to_regress = {
			tr_context_literal = context_literal;
			tr_context_literal_id = context_id;
			tr_clause_literal = clause_literal;
			tr_regress_info = literal_info';
		      }
		      in			
			find_uip'
			  (index + 1)
			  (To_regress.add new_to_regress to_regress)
	    end
	end
      in
	find_uip state context regress_stop (find_uip' 0 to_regress)
    end





(*** regression ***)


(* add the regressed clause literal of a context literal to the regressed set *)
let add_regressed (regressed: regressed) (context_literal: literal) (clause_literal: literal) (offset: int) : unit =
  if regressed.regressed_size >= Const.lemma_max_constraints then begin
    raise LIMIT;
  end
  else begin
    regressed.regressed_size <- regressed.regressed_size + 1;
  end;

  let old =
    try
      LiteralTable.find regressed.regressed_literals context_literal
    with
      | Not_found ->
	  let old =
	    ref []
	  in
	    LiteralTable.add regressed.regressed_literals context_literal old;
	    old
	
  in
    old := (clause_literal, offset) :: !old

let add_fd_constraint (regressed: regressed) (clause_literal: literal) (offset: int) : unit =
  add_regressed regressed regressed_fd_literal clause_literal offset




(* ensure that the dependencies of a context literal are already regressed.

   if all its dependencies are already regressed,
   i.e. are not to be regressed (Split) or memoized,
   to_regress is returned unchanged.
*)
let ensure_regressed
  (state: state)
  (context: context)
  (* the id of the uip (context literal) to stop the regression at *)
  (uip: int option)
  (* only regress context literals up to this choice point *)
  (regress_stop: choice_point)
  (* branch nodes to regress * offset *)
  (to_regress: To_regress.t)
  (* the actual context literal to regress *)
  (current_to_regress: to_regress)
  (* the memoization table *)
  memoization
  :
  (* the extended literals to regress.  *)
  To_regress.t
  =

  let rec ensure_regressed' (index: int) (to_regress: To_regress.t) : To_regress.t =
    (* all context literals done *)
    if index >= Array.length current_to_regress.tr_regress_info.State.li_context_literals then begin
      to_regress
    end
    
    (* the regressed assert literal - just ignore it.
       this does not cover a a unit split,
       in that case the context literal instantiated to the split literal is regressed. *)
    else if (Term.literal_equal current_to_regress.tr_regress_info.State.li_context_literals.(index) Term.assert_literal) then begin
    (*else if index == current_to_regress.tr_regress_info.State.li_clause_index then begin*)
      ensure_regressed' (index + 1) to_regress
    end

    (* process this context_literal *)
    else begin
      let clause_literal =
	List.nth current_to_regress.tr_regress_info.State.li_clause index
      in
      let context_literal =
	current_to_regress.tr_regress_info.State.li_context_literals.(index)
      in

	(* already memoized *)
	if MemoizationTable.mem memoization context_literal then begin
	  ensure_regressed' (index + 1) to_regress
	end

	(* no need to memoize -v *)
	else if
	  Term.literal_equal context_literal Term.minus_v
	  ||
	  Term.literal_equal context_literal Term.plus_v
	then begin
	  ensure_regressed' (index + 1) to_regress
	end

	(* more work to do *)
	else begin
	  let explanation =
	    State.get_explanation state [| context_literal |]
	  in
	  let literal_info' =
	    try
	      State.get_literal_info state context_literal
	    with
	      | Not_found ->
		  failwith ("Lemma_lifted.ensure_regressed: context literal " ^ Term.literal_to_string context_literal)
	  in

	    match explanation with
	      | [] ->
		  (* This turned out to be slow:
		     a root assert of a unit clause is to be dropped from the regression.
		     that is, constraints to it may be created,
		     but its regression is empty. *)
		  begin
(*		    (*match literal_info'.State.li_clause with
		      | clause_literal' :: [] when*)
		    match literal_info'.State.li_type with
		      | State.Assert when
			  State.choice_point_equal literal_info'.State.li_choice_point (State.root_choice_point state) ->
			  let clause_literal' =
			    List.nth literal_info'.State.li_clause literal_info'.State.li_clause_index
			  in
			  (*&&
			    Unification.is_term_instance clause_literal.Term.term clause_literal'.Term.term ->*)
			     let memoized =
			       {
				 mm_clause_literal = clause_literal';
				 mm_max_offset = 0;
				 mm_ground = [];
				 mm_regressed = LiteralTable.create 0;
				 mm_constraints = [];
				 mm_disconnected = false;
			       }
			     in	  
			       MemoizationTable.add memoization context_literal memoized;
			       ensure_regressed' (index + 1) to_regress

		      (* don't memoize, tread as ordinary not regressed assert literal *)
		      | _ ->*)
			  ensure_regressed' (index + 1) to_regress
		  end

	      | creation_point :: _ when
		  (State.compare_age regress_stop creation_point > 0) ->
		  (* no need to memoize an assert older than the choice points to regress. *)
		  ensure_regressed' (index + 1) to_regress

	      | _ ->
		    (* no need to memoize a Split.
		       this does not catch Unit Splits, as those have type Assert. *)
		    if
		      literal_info'.State.li_type = State.SplitLeft
		      ||
		      literal_info'.State.li_type = State.SplitRight
		    then begin
		      ensure_regressed' (index + 1) to_regress
		    end
		      
		    (* the normal case, an actual assert literal to regress *)
		    else begin
		      let context_id =
			match Context.element_for_literal context context_literal with
			  | Some element -> element.Context.el_id
			      
			  | None -> failwith "Lemma_lifted.ensure_regressed: context_id"
		      in
			(* ignore if outside uip *)
			if match uip with
			  | Some id -> id > context_id
			  | None -> false
			then begin
			  ensure_regressed' (index + 1) to_regress
			end

			(* add context literal to regress *)
			else begin
			  if debug_print then print_endline ("\nto regress:");
			  if debug_print then print_endline ("context_literal: " ^ Term.literal_to_string context_literal);
			  if debug_print then print_endline ("clause_literal: " ^ Term.literal_to_string clause_literal);
			  if debug_print then print_endline ("clause: " ^ Term.clause_to_string literal_info'.State.li_clause);
			  if debug_print then print_endline ("constrained: " ^ Term.clause_to_string literal_info'.State.li_constrained);
			  let new_to_regress = {
			    tr_context_literal = context_literal;
			    tr_context_literal_id = context_id;
			    tr_clause_literal = clause_literal;
			    tr_regress_info = literal_info';
			  }
			  in
			    ensure_regressed'
			      (index + 1)
			      (To_regress.add new_to_regress to_regress)
			end
		  end
       end
    end
  in
    ensure_regressed' 0 to_regress




(* regress one context literal.
   assumes that all dependencies are already regressed and memoized .*)
let regress_one
  (* the context literal to regress *)
  (current_to_regress: to_regress)
  (* memoization table *)
  (memoization: memoization)
  :
  unit
  =

  (* offset of entry clause literal in memoization is always 0 *)
  let entry_offset =
    0
  in

  (* the max offset of the currently merged regressions. *)
  let max_offset =
    ref 0
  in

  (* the regressed literals of the currently merged regressions. *)
  let regressed = {
    regressed_literals = LiteralTable.create 256;
    regressed_size = 0;
  }
  in

  (* regress the constraints *)
  begin
    List.iter
      (fun literal ->
	 add_fd_constraint regressed literal entry_offset
      )
      current_to_regress.tr_regress_info.State.li_constrained
  end;

  (* merge the memoized regressions of the context literals
     this context literal to regress depends on. *)
  let rec regress_one' (index: int) ground constraints : unit =

    (* all memoizations merged, create and store the memoization of this context literal  *)
    if index >= Array.length current_to_regress.tr_regress_info.State.li_context_literals then begin
      let context_literal =
	current_to_regress.tr_context_literal
      in
	
      (* the clause literal paired with the regressed context literal. *)
      let clause_literal =
	(* pseudo-closing literal *)
	if Term.literal_equal current_to_regress.tr_context_literal Lemma.close_literal then
	  Lemma.close_literal
	else
	  List.nth current_to_regress.tr_regress_info.State.li_clause current_to_regress.tr_regress_info.State.li_clause_index
      in
      let memoized =
	{
	  mm_clause_literal = clause_literal;
	  mm_max_offset = !max_offset;
	  mm_ground = ground;
	  mm_regressed = regressed;
	  mm_constraints = constraints;
	  mm_disconnected = false;
	}
      in	  
	MemoizationTable.add memoization context_literal memoized;

    end

    (* the regressed assert literal - just ignore it *)
    else if (Term.literal_equal current_to_regress.tr_regress_info.State.li_context_literals.(index) Term.assert_literal) then begin
    (*else if index == current_to_regress.tr_regress_info.State.li_clause_index then begin*)
      regress_one' (index + 1) ground constraints
    end
      
    (* merge the next regression *)
    else begin
      let context_literal =
	current_to_regress.tr_regress_info.State.li_context_literals.(index)
      in
      let clause_literal =
	List.nth current_to_regress.tr_regress_info.State.li_clause index
      in
	try
	  (* use memoization *)
	  let memoized =
	    MemoizationTable.find memoization context_literal
	  in
	    
	    (* a disconnected regression already been used *)
	    if memoized.mm_disconnected then begin
	      let constraints' =
		(* no need to add constraint between ground literals *)
		if Term.is_literal_ground clause_literal && Term.is_literal_ground memoized.mm_clause_literal then
		  constraints
		else
		  (* offset of entry literal does not matter as ground *)
		  ((clause_literal, entry_offset), (memoized.mm_clause_literal, 0)) :: constraints
	      in
		regress_one' (index + 1) ground constraints'
	    end

	    (* use memoization by renaming the offsets.
	       this is done by simply adding the current max_offset to the offsets used in the memoization. *)
	    else begin
	      let ground =
		memoized.mm_ground @ ground
	      in
		(* create variants of regressed literals *)
		LiteralTable.iter
		  (fun context_literal literals ->
		     List.iter
		     (fun (clause_literal, offset) ->
			add_regressed regressed context_literal clause_literal (offset + !max_offset + 1)
		     )
		     !literals
		  )
		  memoized.mm_regressed.regressed_literals;

		(* create variants of regressed constraints *)
		let constraints' =
		  List.fold_left
		    (fun acc ((literal, offset), (literal', offset')) ->
		       ((literal, (offset + !max_offset + 1)), (literal', (offset' + !max_offset + 1))) :: acc
		    )
		    constraints
		    memoized.mm_constraints
		in
		let constraints' =
		  (* first disconnected use *)
		  if Term.is_literal_ground memoized.mm_clause_literal then begin
		    memoized.mm_disconnected <- true;
		  end;

		  (* no need to add constraint between ground literals *)
		  if Term.is_literal_ground clause_literal && Term.is_literal_ground memoized.mm_clause_literal then
		    constraints'
		  else
		    ((clause_literal, entry_offset), (memoized.mm_clause_literal, (!max_offset + 1))) :: constraints'
		in
		  max_offset := !max_offset + memoized.mm_max_offset + 1;
		  regress_one' (index + 1) ground constraints'
	    end
	with
	  | Not_found ->
	      (* a context literal which is not to be regressed,
		 so just use the clause literal instead. *)

	      (* in order for simplification we need to group all instances of a context literal.
	         that is, the clause literal being ground does not suffice
		 to just put it to the ground regressed set,
		 the context literal has to be ground. *)
	      if Term.is_literal_ground context_literal && Term.is_literal_ground clause_literal then
		regress_one' (index + 1) (clause_literal :: ground) constraints
	      else begin
		add_regressed regressed context_literal clause_literal entry_offset;
		regress_one' (index + 1) ground constraints
	      end
    end
 in
  regress_one' 0 [] []



(* regress all context literals to regress *)
let rec regress'
  (state: state)
  (context: context)
  (* the id of the uip (context literal) to stop the regression at *)
  (uip: int option)
  (* only regress context literals up to this choice point *)
  (regress_stop: choice_point)
  (* context literals to regress *)
  (to_regress: To_regress.t)
  (* memoization table *)
  (memoization: memoization)
  :
  unit
  =

  (* nothing more to regress *)
  if To_regress.is_empty to_regress then begin
    ()
  end

  (* add constraint and regress *)
  else begin    
    (* depth first, so regress the oldest context literal first *)
    let next =
      To_regress.min_elt to_regress
    in
    (* are all its dependencies already regressed? *)
    let to_regress' =
      ensure_regressed state context uip regress_stop to_regress next memoization
    in
      (* no new regression tasks,
	 so all dependent regression for this context literla done,
	 so process it *)
      if to_regress == to_regress' then begin
	let tail =
	  To_regress.remove next to_regress'
	in
	  regress_one next memoization;
	  regress' state context uip regress_stop tail memoization
      end
	
      (* new dependent regressions need to be done first. *)
      else begin
	regress' state context uip regress_stop to_regress' memoization
      end
	
  end






(* wrapper for regress'.
   regresses all context literals to regress. *)
let regress
  (state: state)
  (context: context)
  (* the id of the uip (context literal) to stop the regression at *)
  (uip: int option)
  (* only regress context literals up to this choice point *)
  (regress_stop: choice_point)
  (* clause literal * offset contained in the regressed literal set *)
  (to_regress: To_regress.t)
  :
  memoized
  =
  let memoization =
    MemoizationTable.create 1024
  in
    regress'
      state context
      uip regress_stop
      to_regress
      memoization;
    
    try
      MemoizationTable.find memoization Lemma.close_literal
    with
      | Not_found ->
	  failwith "Lemma_lifted.regress: root not memoized"



(*** main function ***)


let get_lemma ~(uip: bool) (state: state) (context: context) context_unifier (clause: clause) (constrained: clause) (conflict_literals: literal array) regress_stop : clause option =
if debug_print then print_endline ("\n\nget_lemma");
try
  (* fake data to get the regression with the closing clause started *)
  let closing_info =
    State.create_literal_info Lemma.close_literal State.Propagation
      (State.active_choice_point state) context_unifier
      clause constrained [] conflict_literals (-1) (fun () -> ()) (Some [regress_stop])
  in
  let regression_start = {
    tr_clause_literal = Lemma.close_literal;
    tr_context_literal = Lemma.close_literal;
    (* pseudo-context literal: most recent context element *)
    tr_context_literal_id = Pervasives.max_int;
    tr_regress_info = closing_info;
  }
  in

  let uip =
    if uip then begin
      let uip =
	find_uip state context
	  regress_stop
	  (To_regress.add regression_start To_regress.empty)
      in
	Some uip
    end

    else
      None
  in


  (* do the regression *)
  let memoized =
    regress
      state context
      uip regress_stop
      (To_regress.add regression_start To_regress.empty)
  in

  let regressed_literals, constraints, ground =
    memoized.mm_regressed, memoized.mm_constraints, memoized.mm_ground
  in
        if debug_print then begin
	  print_endline ("\nconstraints:");
    List.iter
      (fun ((a, a0), (b, b0)) ->
	 print_endline (
	   (string_of_int a0 ^ " : " ^ Term.literal_to_string a)
	   ^ "  ---  " ^
	   (string_of_int b0 ^ " : " ^ Term.literal_to_string b)
	 )
      )
      constraints;

    print_endline ("\nground:");
    List.iter
      (fun literal ->
	 print_endline (Term.literal_to_string literal)
      )
      ground;
	end;
  (* create the conflict set by applying the constraints to the regressed literals *)
  let regressed =
    LiteralTable.fold
      (fun _ literals acc ->
	 !literals :: acc
      )
      regressed_literals.regressed_literals
      []
  in
  let unifier =
    Unification.unify_constraints' ~recompute:true constraints
  in
  let regressed =
    Subst.apply_to_literal_groups' unifier regressed
  in
if debug_print then begin
  print_endline ("\nregressed:");
    List.iter
      (fun literals ->
	 List.iter
	   (fun literal ->
	      print_endline (Term.literal_to_string literal)
	   )
	   literals
      )
      regressed;
end;
  let simplified =
(*    Term.remove_duplicates (ground @ (Lemma.simplify regressed))*)
    Term.remove_duplicates (Term.remove_variants (ground @ (Lemma.simplify regressed)))
  in

    if debug_print then print_endline ("Simplified: " ^ Term.clause_to_string simplified);

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
with
  | LIMIT ->
      None
