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


type config = Config.config
type bound = Bound.bound
type literal = Term.literal
type clause = Term.clause
type state = State.state
type context = Context.context
type choice_point = State.choice_point
type explanation = State.explanation
type propositional_lemmas
type raw_context_unifier = Context_unifier.raw_context_unifier
type candidates = Selection_assert.candidates
type selected = Selection_types.selected
type space = Context_unifier.context_unifier_space
type space_registry = Context_unifier_space.space_registry

type status =
  | True of Context.element
  | False of Context.element (* the paired context literal *)
  | Undetermined

type abstraction = {
  (* abstraction of this literal *)
  abs_literal: literal;

  mutable abs_status: status;
}

module LiteralTable = Term.LiteralTable

module AbstractionTable =
  Hashtbl.Make (
    struct
      type t = abstraction
	  
      let equal x y : bool =
	Term.literal_equal x.abs_literal y.abs_literal
	  
      let hash x : int =
	Term.hash_of_literal x.abs_literal
		
    end
  )

class index_data =
object
  method is_equal (x: abstraction) (y: abstraction) =
    x == y
      
  method to_string abstraction : string =
    Term.literal_to_string abstraction.abs_literal
end

type predicate_index = abstraction Term_indexing.predicate_index
type index = abstraction Term_indexing.index


type lemma = {
  (* the 'lemma' clause,
     i.e. a set of context literals whose negated conjunction
     is inconsistent with the problem clause set. *)
  clause: clause;
  (* space data structure over the clause to be able to perform assert/close *)
  space: space;
  (* if not true than the lemma depends on the current derivation
     and can not be reused for another derivation (over a restart). *)
  global: bool;
  (* the abstraction and status of each clause literal *)
  abstraction: abstraction array;
  (* the two watched literals *)
  mutable watched1: int;
  mutable watched2: int;
}

module LemmaTable =
  Hashtbl.Make (
    struct
      type t = lemma
	  
      let equal x y : bool =
	x == y
	  
      let hash x : int =
	x.space.Context_unifier.cus_id
		
    end
  )

module ClauseTable = Term.ClauseTable

type lemmas_propositional = {
  (* environment *)
  config: config;
  state: state;
  context: context;
  space_registry: space_registry;

  (* to propagate candidates *)
  candidates: candidates;
  process_close: (raw_context_unifier -> unit);
  process_assert: (raw_context_unifier -> bool);

  (* mapping literals and from/to abstractions *)
  literal_to_abstraction: abstraction LiteralTable.t;
  abstraction_to_lemmas: (unit LemmaTable.t) AbstractionTable.t;
  (* all lemmas *)
  lemmas: lemma ClauseTable.t;

  (* abstract literals to the same abstraction,
     if they generalize to the same literal *)
  subsumed: index;

  (* computed assert propagations on the lemmas *)
  mutable propagations: (choice_point * lemma) list;
}

let create config state context candidates process_close process_assert space_registry =
  if Const.debug && Config.finite_domain config then
    failwith "Lemma_propositional.create: finite domain mode not supported.";
  {
  config = config;
  state = state;
  context = context;
  space_registry = space_registry;

  candidates = candidates;
  process_close = process_close;
  process_assert = process_assert;

  literal_to_abstraction = LiteralTable.create 1023;
  abstraction_to_lemmas = AbstractionTable.create 1023;
  lemmas = ClauseTable.create 51;

  subsumed = Discrimination_tree.create_index false (new index_data);

  propagations = [];
}



(*** store lemmas ***)


(* remove old (bad) lemmas if there are too many *)
let prune_lemmas lemmas_propositional : unit =
  let max_lemmas = Config.lemma_max lemmas_propositional.config
  and min_lemmas = Config.lemma_min lemmas_propositional.config
  in
  (* are there too many lemmas? *)
  if max_lemmas > 0 && ClauseTable.length lemmas_propositional.lemmas >= max_lemmas then begin

    let rec find_min min =
      let pruned =
	ClauseTable.fold
	  (fun _ lemma pruned ->
	     if !(lemma.space.Context_unifier.cus_utility) > min then
	       pruned
	     else
	       pruned + 1
	  )
	  lemmas_propositional.lemmas
	  0
      in
	if pruned >= (max_lemmas - min_lemmas) then
	  pruned
	else
	  find_min (min + 1)
    in
    let min =
      find_min 0
    in

    let _, remove =
      ClauseTable.fold
	(fun clause lemma ((pruned, remove) as acc) ->
	   if
	     (pruned >= (max_lemmas - min_lemmas))
	     ||
	     (!(lemma.space.Context_unifier.cus_utility) > min)
	   then begin
	     acc
	   end
	     
	   else begin
	     (pruned + 1, clause :: remove)
	   end
	)
	lemmas_propositional.lemmas
	(0, [])
    in
      List.iter
	(fun clause -> ClauseTable.remove lemmas_propositional.lemmas clause)
	remove
  end


let get_lemmas lemmas_propositional =
  List.rev (
  ClauseTable.fold
    (fun _ lemma acc ->
       if lemma.global then
	 lemma.clause :: acc

       else
	 acc
    )
    lemmas_propositional.lemmas
    []
  )

(* ignore a lemma if it is a variant of an already existing clause or lemma.
   this is only an incomplete test as we don't want to do a full subsumption test. *)
let filter_lemma lemmas_propositional (clause: clause) : bool =
    (* variant of existing lemma *)
    ClauseTable.mem
      lemmas_propositional.lemmas
      clause


(*** watched literals ***)

let register_watched_literal lemmas_propositional abstraction lemma =
  let lemmas =
    try
      AbstractionTable.find lemmas_propositional.abstraction_to_lemmas abstraction
    with
      | Not_found ->
	  let lemmas =
	    LemmaTable.create 63
	  in
	    AbstractionTable.add lemmas_propositional.abstraction_to_lemmas abstraction lemmas;
	    lemmas
	      
  in
    LemmaTable.add lemmas lemma ()

let unregister_watched_literal lemmas_propositional abstraction lemma =
  let lemmas =
    try
      AbstractionTable.find lemmas_propositional.abstraction_to_lemmas abstraction
    with
      | Not_found ->
	  failwith "unregister_watched_literal"
  in
    LemmaTable.remove lemmas lemma



let rec find_watched_literal abstraction i j =
    (* take last one, subsumed or not *)
    if i >= Array.length abstraction then
      i

    else if i == j then
      find_watched_literal abstraction (i + 1) j
	
    else match abstraction.(i).abs_status with
      | True _ ->
	  (*failwith "new lemma already satisfied"*)
	  i
	    
      | False _ ->
	  find_watched_literal abstraction (i + 1) j
	    
      | Undetermined ->
	  i




(*** propagations ***)

	    
let compute_context_unifier lemma =
  let context_partners =
    Array.map
      (fun abstraction ->
	 match abstraction.abs_status with
	     
	   | False element ->
	       Context_unifier.create_context_partner
		 element
		 None
		 true		   
		 
	   | Undetermined ->
	       Context_unifier.assert_partner;			
	       
	   | True _ ->
	       failwith "compute_propagation: not inconsistent but subsumed"
      )
      lemma.abstraction
  in
    Context_unifier.create_context_unifier
      lemma.space
      context_partners
      false
      None (* finite domain mode not supported *)


let rec compute_propagation lemmas_propositional lemma =
  match lemma.abstraction.(lemma.watched1).abs_status, lemma.abstraction.(lemma.watched2).abs_status with
    | True _, _
    | _, True _ ->
	(* subsumed *)
	()

    | False _, Undetermined ->
	(* find new watched literal *)
	let i =
	  find_watched_literal lemma.abstraction 0 lemma.watched2
	in
	  (* only one undecided left *)
	  if i = Array.length lemma.abstraction then begin
	    lemmas_propositional.propagations <-
	      (State.active_choice_point lemmas_propositional.state, lemma) :: lemmas_propositional.propagations;
	    ignore (lemmas_propositional.process_assert (compute_context_unifier lemma): bool)
	  end

	  else begin
	    unregister_watched_literal lemmas_propositional lemma.abstraction.(lemma.watched1) lemma;
	    lemma.watched1 <- i;
	    register_watched_literal lemmas_propositional lemma.abstraction.(lemma.watched1) lemma;
	  end

    | Undetermined, False _ ->
	(* find new watched literal *)
	let i =
	  find_watched_literal lemma.abstraction 0 lemma.watched1
	in
	  (* only one undecided left *)
	  if i = Array.length lemma.abstraction then begin
	    lemmas_propositional.propagations <-
	      (State.active_choice_point lemmas_propositional.state, lemma) :: lemmas_propositional.propagations;
	    ignore (lemmas_propositional.process_assert (compute_context_unifier lemma): bool)
	  end

	  else begin
	    unregister_watched_literal lemmas_propositional lemma.abstraction.(lemma.watched2) lemma;
	    lemma.watched2 <- i;
	    register_watched_literal lemmas_propositional lemma.abstraction.(lemma.watched2) lemma;
	  end
	

    | False _, False _ ->
	(* find new watched literal *)
	let i =
	  find_watched_literal lemma.abstraction 0 (-1)
	in
	  (* inconsistent *)
	  if i = Array.length lemma.abstraction then begin
	      raise (Context_unifier.CLOSE (compute_context_unifier lemma))
	  end
	  else begin
	    unregister_watched_literal lemmas_propositional lemma.abstraction.(lemma.watched1) lemma;
	    lemma.watched1 <- i;
	    register_watched_literal lemmas_propositional lemma.abstraction.(lemma.watched1) lemma;
	  end;
	  compute_propagation lemmas_propositional lemma

    | Undetermined, Undetermined ->
	if Array.length lemma.abstraction = 1 then begin
	    lemmas_propositional.propagations <-
	      (State.active_choice_point lemmas_propositional.state, lemma) :: lemmas_propositional.propagations;
	    ignore (lemmas_propositional.process_assert (compute_context_unifier lemma): bool)
	end
	else begin
	  ()
	end





let add_lemma lemmas_propositional clause global =
  (* first normalize all variables, independent of each other.
     this is the clause used for variant elimination *)
  let clause' = 
    Term.sort_clause (
    List.map
      (fun literal -> Subst.apply_to_literal Subst.empty literal 0)
      clause
    )
  in

  if filter_lemma lemmas_propositional clause' then begin
    ()

  end
  else begin
  prune_lemmas lemmas_propositional;

  (* now skolemize clause, this is the claus used for propagation *)
  let clause =
    Term.request_skolemized_clause clause'
  in
  (* find abstraction of clause and remove variants.
     E.g. p(x) \/ p(y) becomes p(x) *)
  let abstraction =
    List.fold_left
      (fun acc literal ->
	 let literal =
	   Subst.apply_to_literal Subst.empty literal 0
	 in
	 try
	   let abstraction =
	     LiteralTable.find lemmas_propositional.literal_to_abstraction literal
	   in
	     Tools.list_add (==) acc abstraction
	 with
	   | Not_found ->
	       (* can only be false or undetermined *)
	       let status =
		 match Context.check_contradictory lemmas_propositional.context literal with
		   | None ->
		       Undetermined
		   | Some element ->
		       False element
	       in
	       let abstraction = {
		 abs_literal = literal;
		 abs_status = status;
	       }
	       in
      		 LiteralTable.add lemmas_propositional.literal_to_abstraction literal abstraction;
		 (lemmas_propositional.subsumed#find literal)#add literal.Term.atom abstraction;
		 abstraction :: acc
      )
      []
      clause
  in
  let abstraction =
    Array.of_list (List.rev abstraction)
  in

  (* ignore duplicates *)

  (* create the context_unifier_space of this lemma -
     just to later on propagate a candidate to the assert candidat set. *)
  let input_partners =
    Array.mapi
      (fun i abstraction ->
	 Context_unifier.create_input_partner
	   i
	   abstraction.abs_literal
	   []
      )
      abstraction
  in
  let space =
    Context_unifier.create_space
      (Context_unifier_space.get_id lemmas_propositional.space_registry)
      clause
      [] (* considered to be ground *)
      []
      []
      input_partners
      [| |] (* no ordering ever needed *)
      lemmas_propositional.process_close
      lemmas_propositional.process_assert
      (fun _ -> ()) (* no split candidates computed *)
      (fun _ -> false) (* not used in recomputation of candidates *)
      (fun _ -> false) (* not used in recomputation of candidates *)
      true          (* this is a lemma *)
      None (* no constraints *)
  in

  let lemma = {
    clause = clause;
    space = space;
    global = global;
    abstraction = abstraction;
    watched1 = 0;
    watched2 = (-1);
  }
  in

  (* register watched literals *)

    if Array.length abstraction > 1 then begin
      lemma.watched1 <- find_watched_literal abstraction 0 (-1);
      lemma.watched2 <- find_watched_literal abstraction (lemma.watched1 + 1) lemma.watched1;

      if lemma.watched1 == Array.length abstraction then begin
	failwith "new lemma: all literals falsified"
      end;

      (* for bad lemmas this need not to happen,
	 as one literal is instantiated to several. *)
      if lemma.watched2 = Array.length abstraction then begin
	if lemma.watched1 = 0 then
	  lemma.watched2 <- 1
	else
	  lemma.watched2 <- 0;

	register_watched_literal lemmas_propositional lemma.abstraction.(lemma.watched1) lemma;
	register_watched_literal lemmas_propositional lemma.abstraction.(lemma.watched2) lemma;

	(* check if this lemma creates a unit propagation *)
	compute_propagation lemmas_propositional lemma;
      end

      else begin
	register_watched_literal lemmas_propositional lemma.abstraction.(lemma.watched1) lemma;
	register_watched_literal lemmas_propositional lemma.abstraction.(lemma.watched2) lemma;
      end;

    end

    else begin
      lemma.watched2 <- 0;
      register_watched_literal lemmas_propositional lemma.abstraction.(lemma.watched1) lemma;
      compute_propagation lemmas_propositional lemma;
    end;


    (* register lemma *)
    ClauseTable.add lemmas_propositional.lemmas clause' lemma;


    ()
  end

    
  



let extend_context lemmas_propositional element =
  let literal =
    element.Context.el_literal
  in
  let contradictory =
    (lemmas_propositional.subsumed#find (Term.request_negated_literal literal))#find_all_instances
      ~p_preserving:true literal.Term.atom
  in
    List.iter
      (fun abstraction ->
	 match abstraction.abs_status with
	   | True _ ->
	       print_endline (Term.literal_to_string abstraction.abs_literal ^ " : True");
	       failwith "extend_context 1"

	   | False _ ->
	       ()

	   | Undetermined ->
	       abstraction.abs_status <- False element;

	       begin
		 try
		   let lemmas =
		     AbstractionTable.find lemmas_propositional.abstraction_to_lemmas abstraction
		   in
		     LemmaTable.iter
		       (fun lemma _ -> compute_propagation lemmas_propositional lemma)
		       lemmas
		 with
		   | Not_found ->
		       (* not used as a watched literal *)
		       () 
	       end
      )
      contradictory;

  let subsumed =
    (lemmas_propositional.subsumed#find literal)#find_all_instances
      ~p_preserving:true literal.Term.atom
  in
    List.iter
      (fun abstraction ->
	 match abstraction.abs_status with
	   | True _ ->
	       ()

	   | False _ ->
	       failwith "extend_context 2"

	   | Undetermined ->
	       abstraction.abs_status <- True element;
      )
      subsumed;

    ()


(*** backtracking ***)

(* backtrack to the most recent valid choice point *)
let backtrack lemmas_propositional =
  LiteralTable.iter
    (fun _ abstraction ->
       match abstraction.abs_status with
	 | True element
	 | False element when
	     State.is_choice_point_invalid element.Context.el_choice_point ->
	     abstraction.abs_status <- Undetermined

	 | _ ->
	     ()
    )
    lemmas_propositional.literal_to_abstraction;


  (* remove retracted propagations and see if they are still valid. *)
  let keep, remove =
    List.partition
      (fun (cp, _) ->
	 if State.is_choice_point_valid cp then
	   true

	 else
	   false
      )
      lemmas_propositional.propagations
  in
    lemmas_propositional.propagations <- keep;
    List.iter
      (fun (_, lemma) ->
	 compute_propagation lemmas_propositional lemma
      )
      remove

