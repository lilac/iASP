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

type counter = Counter.counter
type config = Config.config
type literal = Term.literal
type clause = Term.clause
type subst = Subst.subst

(* explanations are alway kept decreasingly sorted,
   i.e. youngest choice points first, oldest choice points at the end of the list *)
type explanation = choice_point list

and choice_point = {
  (* age, where 0 is oldest *)
  cp_id: int;

  (* the creating left split literal. *)
  cp_split_literal: literal;

  (* the right split literals applied in this choice point *)
  mutable cp_right_splits: literal list;

  (* still valid, or already retracted and thus invalidated? *)
  mutable cp_valid: bool;
}

type literal_type =
  | Propagation
  | SplitLeft
  | SplitRight

type literal_info = {
  li_literal: literal;
  li_type: literal_type;
  li_choice_point: choice_point;
  li_context_unifier: subst;
  li_clause: clause;
  li_constrained: clause;
  li_clause_vars: Subst.var list;
  li_context_literals: literal array;
  li_clause_index: int;
  li_clause_utility: unit -> unit;
  mutable li_explanation: explanation option;
}


module LiteralTable = Term.LiteralTable


type state = {
  (* environment *)
  st_config: config;

  (* ever increasing choice point id counter. *)
  st_id: counter;

  (* the pseudo-root choice point, i.e. unit asserts *)
  st_root_choice_point: choice_point;

  (* the derivation branch *)
  mutable st_branch: choice_point list;

  (* mapping from a context literal its information *)
  st_literal_info: literal_info LiteralTable.t;
}





(*** constants ***)

let empty_explanation : explanation =
  []

let valid_choice_point : choice_point = {
  cp_id = -1;
  cp_split_literal = Term.null_literal;
  cp_right_splits = [];
  cp_valid = true;
}

let invalid_choice_point : choice_point = {
  cp_id = -2;
  cp_split_literal = Term.null_literal;
  cp_right_splits = [];
  cp_valid = false;
}






(*** creation ***)

let create (config: config) : state =
  let root_choice_point = {
    cp_id = 0;
    cp_split_literal = Term.null_literal;
    cp_right_splits = [];
    cp_valid = true;
  }
  in

  let state =
    {
      st_config = config;
      st_id = Counter.create_with 0;
      st_root_choice_point = root_choice_point;
      st_branch = [root_choice_point];
      st_literal_info = LiteralTable.create 1024;
    }
  in

  let root_branch_node = {
    li_literal = Term.null_literal;
    li_type = Propagation;
    li_choice_point = root_choice_point;
    li_context_unifier = Subst.empty;
    li_clause = [];
    li_constrained = [];
    li_clause_vars = [];
    li_clause_utility = (fun () -> ());
    li_clause_index = -2; (* just some number to crash on if ever used *)
    li_context_literals = [| |]; (* no dependency *)
    li_explanation = Some []; (* no explanation *)
  }
  in
    
    (* add special literals with no dependencies. *)
    LiteralTable.add state.st_literal_info Term.assert_literal root_branch_node;
    LiteralTable.add state.st_literal_info (Config.default_v state.st_config) root_branch_node;
    LiteralTable.add state.st_literal_info Term.null_literal root_branch_node;
    
    state



let create_literal_info literal literal_type choice_point context_unifier clause constrained clause_vars
    context_literals clause_index utility explanation =
  {
    li_literal = literal;
    li_type = literal_type;
    li_choice_point = choice_point;
    li_context_unifier = context_unifier;
    li_clause = clause;
    li_constrained = constrained;
    li_clause_vars = clause_vars;
    li_clause_utility = utility;
    li_clause_index = clause_index;
    li_context_literals = context_literals;
    li_explanation = explanation;
  }


(*** access ***)

let root_choice_point (state: state) : choice_point =
  state.st_root_choice_point


(* the active choice point, i.e. the youngest choice point *)
let active_choice_point (state: state) : choice_point =
  match state.st_branch with
    | [] ->
	failwith "State.active_choice_point"

    | head :: _ ->
	head




(*** representation ***)

let id_to_string (id: int) =
  "[" ^ string_of_int id ^ "] "
  
let choice_point_to_string (choice_point: choice_point) : string =
  id_to_string choice_point.cp_id(* ^ " - " ^ Term.literal_to_string choice_point.cp_split_literal*)

let active_choice_point_to_string (state: state) : string =
  "[" ^ string_of_int (active_choice_point state).cp_id ^ "] "

let choice_point_to_string_extended (choice_point: choice_point) : string =
  "[" ^ string_of_int choice_point.cp_id ^ "] :"
  ^ "\nValid: " ^ string_of_bool choice_point.cp_valid

let explanation_to_string (explanation: explanation) : string =
  "[" ^ String.concat ", " (List.map choice_point_to_string explanation) ^ "]"

    

let branch_to_string (state: state) : string =
  List.fold_left
    (fun acc choice_point ->
       choice_point_to_string_extended choice_point ^ "\n" ^ acc
    )
    ""
    state.st_branch





(*** Comparison ***)

let choice_point_equal (first: choice_point) (second: choice_point) : bool =
  first == second


let compare_age (first: choice_point) (second: choice_point) : int =
  Tools.compare_int first.cp_id second.cp_id

(* as we use purely chronologically backtracking each choice point simply
   depends on any older one and itself *)
let backtracking_depends_on (dependent: choice_point) (base: choice_point) : bool =
  compare_age base dependent <= 0





(*** properties ***)

let id_of_choice_point (choice_point: choice_point) : int =
  choice_point.cp_id

let is_choice_point_valid (choice_point: choice_point) : bool =
  choice_point.cp_valid

let is_choice_point_invalid (choice_point: choice_point) : bool =
  not choice_point.cp_valid

let invalidate_choice_point (choice_point: choice_point) : unit =
  if Const.debug && (choice_point == valid_choice_point) then begin
    failwith "State.invalidate_choice_point: valid_choice_point";
  end;
      
  choice_point.cp_valid <- false

let left_split_literal (choice_point: choice_point) : literal =
  choice_point.cp_split_literal

let right_split_literals (choice_point: choice_point) : literal list =
  List.rev choice_point.cp_right_splits

let get_branch (state: state) : choice_point list =
  state.st_branch

let set_branch (state: state) (branch: choice_point list) : unit =
  state.st_branch <- branch








(*** explanations ***)


(* invariant: explanations are always kept decreasingly sorted *)
let rec merge_explanations (first: explanation) (second: explanation) : explanation =
  if first == second then
    first
  else
    match first, second with
      | [], _ ->
	  second
	    
      | _, [] ->
	  first
	    
      | head1 :: tail1, head2 :: tail2 ->
	  if Const.debug && (is_choice_point_invalid head1 || is_choice_point_invalid head2) then
	    failwith "merge_explanations";
	  
	  begin
	    if head1.cp_id == head2.cp_id then
	      head1 :: merge_explanations tail1 tail2
	    else if head1.cp_id < head2.cp_id then
	      head2 :: merge_explanations first tail2
	    else
	      head1 :: merge_explanations tail1 second
	  end


let rec get_explanation (state: state) (literals: literal array) : explanation =
  Array.fold_left
    (fun acc literal ->
       let literal_info =
	 try
	   LiteralTable.find state.st_literal_info literal
	 with
	   | Not_found ->
	       failwith ("State.get_explanation: " ^ Term.literal_to_string literal)
       in
(*       print_endline (Term.literal_to_string literal);
	 Array.iter
	   (fun literal -> print_string (Term.literal_to_string literal); print_string "   ")
	   literal_info.li_context_literals;
	 print_newline ();*)
       let explanation =
	 match literal_info.li_explanation with
	   | Some explanation ->
	       explanation
		 
	   | None ->
	       let explanation =
		 get_explanation state literal_info.li_context_literals
	       in
		 (* memoize *)
		 literal_info.li_explanation <- Some explanation;
		 explanation
       in
	 merge_explanations acc explanation
    )
    []
    literals






(*** application ***)


let apply_split_left (state: state) (literal: literal) (context_unifier: subst)
    (clause: clause) (constrained: clause) (clause_vars: Subst.var list) (clause_index: int)
    (clause_value: unit -> unit)    
    : choice_point =

  let id =
    try
      Counter.next state.st_id
    with
      | Counter.OVERFLOW ->
	  raise (Const.NO_SOLUTION "State.apply_split_left: state id overflow")
  in
  let new_choice_point = {
    cp_id = id;
    cp_split_literal = literal;
    cp_right_splits = [];
    cp_valid = true;
  }
  in

  let branch_node =
    create_literal_info literal SplitLeft new_choice_point
      context_unifier clause constrained clause_vars [| |] clause_index clause_value
      (Some ([new_choice_point]))
  in
    LiteralTable.add state.st_literal_info literal branch_node;

    (* extend the branch *)
    state.st_branch <- new_choice_point :: state.st_branch;
    new_choice_point


let apply_split_right (state: state) (literal: literal) (context_unifier: subst)
    (clause: clause) (constrained: clause) (clause_vars: Subst.var list) (clause_index: int)
    (clause_value: unit -> unit) (context_literals: literal array)
    : unit =
  let active =
    active_choice_point state
  in
  let literal_info =
    create_literal_info literal SplitRight active
      context_unifier clause constrained clause_vars context_literals clause_index clause_value None
  in
    active.cp_right_splits <- literal :: active.cp_right_splits;
    LiteralTable.add state.st_literal_info literal literal_info


let apply_propagation (state: state) (literal: literal) (context_unifier: subst)
    (clause: clause) (constrained: clause) (clause_vars: Subst.var list) (clause_index: int)
    (clause_value: unit -> unit)
    (context_literals: literal array)
    : unit =

  let literal_info =
    create_literal_info literal Propagation (active_choice_point state)
      context_unifier clause constrained clause_vars context_literals clause_index clause_value None
  in
    LiteralTable.add state.st_literal_info literal literal_info




(*** literal info ***)


let backtrack_literal_info (state: state) : unit =
  let invalid_keys =
    LiteralTable.fold
      (fun _ literal_info acc ->
	 if is_choice_point_invalid literal_info.li_choice_point then
	   literal_info.li_literal :: acc

	 else
	   acc
      )
      state.st_literal_info
      []
  in
    List.iter
      (LiteralTable.remove state.st_literal_info)
      invalid_keys


let get_literal_info (state: state) (literal: literal) : literal_info =
  LiteralTable.find state.st_literal_info literal
