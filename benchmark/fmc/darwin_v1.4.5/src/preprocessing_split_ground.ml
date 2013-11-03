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

type symbol = Symbol.symbol
type var = Var.var
type term = Term.term
type literal = Term.literal
type clause = Term.clause
type counter = Counter.counter


(*
This follows the paper
Splitting without Backtracking
Alexander Riazanov
Andrei Voronkov

But:
- the partition algorithm is implemented differently.
  mostly, hash tables are used instead of int arrays,
  as this is more convenient,
  and hopefully not much slower.

- labelling is not done on variants, but only on identical clauses.
  as all clauses are ordered and normalized,
  this is hopefully most of the time sufficient for reuse
  and computationally cheaper.

Ground Splitting Rule:
C \/ D
------------------------
C \/ -p, D \/ -q, p \/ q

where:
- C and D are variable disjoint

With Labeling:
C \/ D,  D' \/ -q, p \/ q
------------------------
C \/ -p, p \/ q

where additionally:
-  D is a variant of D'

Note: if the input contains variants of a clause,
then after splitting there will by copies of connection clauses
of the form p \/ q.
*)


(* store the label under which a clause C has been stored.
   i.e. if the clause is p \/ C,
   where p is the label,
   then the request C returns -p.

   clauses are only recognized as equal if the literals
   are identical and in the same order,
   variants are not recognized.

   thus, they should be normalized beforehand.
*)
module Labels = Term.ClauseApproxTable

(* where literal is the label used in the other parts of the clause
   as the connection literal. *)
type labels =
    literal Labels.t


(* the already created connection clauses,
   e.g. p \/ q
*)
module ConnectionClauses = Term.ClauseApproxTable

(* where literal is the label used in the other parts of the clause
   as the connection literal. *)
type connection_clauses =
    unit ConnectionClauses.t




module VarTable = Var.VarTable
module LiteralTable = Term.LiteralTable



(* partition the literals into equivalence classes
   based on variable containment.
   I.e. all literals sharing a variable are in the same class.

   returns a mapping from a class representive (variable)
   to a list of all class members (literals),
   and the list of ground literals separately. *)
let partition (clause: clause) : ((literal list ref) VarTable.t) * (literal list) =

  (* variable to representative mapping *)
  let var_rep : var VarTable.t =
    VarTable.create 64
  in
  (* literal to representative mapping *)
  let literal_rep : var LiteralTable.t =
    LiteralTable.create 64
  in
  (* the ground literals *)
  let ground_literals : literal list ref =
    ref []
  in

  (* get the representative of a variable.
     initially each variable is its own representative,
     i.e. it has no explicit representative *)
  let rec get_var_rep (var: var) : var =
    try
      let rep =
	VarTable.find var_rep var
      in
	(* no path compression, so do it recursively *)
	get_var_rep rep
    with
      | Not_found ->
	  var
  in

    (* put each term in its equivalence class *)
    List.iter
      (fun literal ->
	 (* ignore duplicate literals *)
	 if
	   List.exists (Term.literal_equal literal) !ground_literals
	   ||
	   LiteralTable.mem literal_rep literal
	 then begin
	   ()
	 end

	 else begin
	   match Term.vars_of_literal literal with
	     | [] ->
		 (* ground *)
		 ground_literals := literal :: !ground_literals
		     
	       | var :: tail ->
		   (* point all vars to the representative of the first var *)
		   let rep =
		     get_var_rep var
		   in
		     List.iter
		       (fun var' ->
			  let rep' =
			    get_var_rep var'
			  in
			    if not (Var.equal rep' rep) then
			      VarTable.add var_rep rep' rep
		       )
		       tail;

		     (* this representative is also the representative of the literal *)
		     LiteralTable.add literal_rep literal rep
	 end
      )
      clause;

    (* mapping from representative to all literals in its equivalence class *)
    let eq_classes : (literal list ref) VarTable.t =
      VarTable.create 16
    in
      (* move each literal to the final representative *)
      LiteralTable.iter
	(fun literal rep ->
	   let current =
	     try
	       VarTable.find eq_classes (get_var_rep rep)
	     with
	       | Not_found ->
		   let current =
		     ref []
		   in
		     VarTable.add eq_classes (get_var_rep rep) current;
		     current
	   in
	     current := literal :: !current;
	)
	literal_rep;
      eq_classes, !ground_literals



(* - partition based on variable containment
   - transform partitions to clauses
   - connect clauses by fresh constants
   - resuse old labels
*)
let split_clause (labels: labels) (counter: counter) (connection_clauses: connection_clauses) (clause: clause) : (clause list) option =
  let partitions, ground_literals =
    partition clause
  in
  
    (* only one partition, so do no split *)
    if VarTable.length partitions <= 1 then begin
      None
    end
      
    (* split *)
    else begin
      Counter.inc_by counter (VarTable.length partitions - 1);

      (* transform equivalence classes to clauses *)
      let clauses =
	VarTable.fold
	  (fun _ clause clauses ->
	     !clause :: clauses
	  )
	  partitions
	  []
      in
	
      (* add the ground literals to one clause *)
      let clauses =
	if List.length ground_literals > 0 then
	  match clauses with
	    | [] -> failwith "Preprocessing.split_ground_clause: add ground literals"
		
	    | head :: tail ->
		(ground_literals @ head) :: tail
	else
	  clauses
      in
	  
      (* connect the clauses with fresh connection constants *)
      let rec connect (to_label: clause list) (labelled: clause list) (connection_clause: literal list) : clause list =
	match to_label with
	  | [] ->
	      (* finally also add the connection clause *)
	      if ConnectionClauses.mem connection_clauses connection_clause then begin
		labelled
	      end

	      else begin
		ConnectionClauses.add connection_clauses connection_clause ();
		connection_clause :: labelled
	      end
		
	  | clause :: tail ->
	      (* normalize each new clause before labelling it *)
	      let clause =
		Subst.normalize_clause (Term.sort_clause clause)
	      in
		try
		  (* reuse same clause that has been labelled previously *)
		  let label =
		    Labels.find labels clause
		  in
		    connect tail labelled (label :: connection_clause)
		with
		  | Not_found ->
		      (* create a fresh label *)
		      let label_term =
			Term.request_const (Symbol.create_connection 0)
		      in

		      let left_label, right_label =
			let pos_label =
			  Term.request_literal true label_term
			and neg_label =
			  Term.request_literal false label_term
			in
			  (* ensure that horn clauses are split into horn clauses *)
			  if Term.is_definit clause then
			    neg_label, pos_label

			  (* otherwise, put the negative labes into the connection clause.
			     this turned out to be best. *)
			  else			    
			    pos_label, neg_label
		      in

                      (* neg_label to clause so that no split candidate can be built
			 until the connection clause has been split on. *)
		      let labelled_clause =
			left_label :: clause
		      in
			Labels.add labels clause right_label;
			connect tail (labelled_clause :: labelled) (right_label :: connection_clause)
      in
      let clauses =
	connect clauses [] []
      in
	Some clauses
	    
    end




let split ~(print: bool) (clauses: clause list) : clause list =
  if print then begin
    print_endline ("Preprocessing: Ground Splitting");
  end;

  let labels =
    Labels.create 64
  in
  let connection_clauses =
    ConnectionClauses.create 64
  in
  let counter =
    Counter.create_with 0
  in

  (* split each clause *)
  let rec split' to_split already_split split_done =
    match to_split with
      | [] ->
	  if print then begin
	    print_endline ("Performed " ^ string_of_int (Counter.value counter) ^ " ground splits.");
	    print_newline ();
	  end;

	  (* split done, so reverse the new clauses to get the original clause order *)
	  if split_done then
	    List.rev already_split

	  (* no split done *)
	  else
	    clauses

      | clause :: tail ->
	  begin
	    match split_clause labels counter connection_clauses clause with
	      | None ->
		  split' tail (clause :: already_split) split_done

	      | Some split ->
		  if print then begin
		    print_endline ("Ground split: " ^ Term.clause_to_string clause);
		    List.iter
		      (fun clause ->
			 print_endline ("-> " ^ Term.clause_to_string clause)
		      )
		      split;
		    print_newline ();
		  end;		  

		  split' tail (split @ already_split) true
	  end
  in
    split' clauses [] false
