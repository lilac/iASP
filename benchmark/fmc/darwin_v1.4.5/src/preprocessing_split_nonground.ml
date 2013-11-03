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
New Techniques that Improve MACE-style Finite Model Finding
Koen Classen
Niklas Soerensson

split a clause based on the variable
which is connected to the least number of variables,
where connecte means 'contained in the same clause'.

Splitting Rule:
C \/ D
------------------------
C \/ p, D \/ -p

where:
- p contains all variables shared by C and D
- C contains at least one variable not contained in D
  and vice versa.

With Labeling:
C \/ D,  C' \/ p
----------------
-p \/ D

where additionally:
- C is a variant of C'
- after simultaneously renaming C' and p to C,
  the variables of p are exactly the variables shared by C and D
*)


(* store the label under which a clause C has been stored.
   i.e. if the clause is p(x0, ..., xb) \/ C,
   where p is the label,
   then the request C returns -p.

   clauses are only recognized as equal if the literals
   are identical and in the same order,
   variants are not recognized.

   thus, they should be normalized beforehand.
*)
module Labels =
  Hashtbl.Make (
    struct
      type t = literal * clause
	  
      let equal (label0, clause0) (label1, clause1): bool =
	(* label uses the same clause variables *)
	begin
	  match label0.Term.atom, label1.Term.atom with
	    | Term.Func func0, Term.Func func1 ->
		Tools.array_for_all2
		  Term.term_equal
		  func0.Term.subterms
		  func1.Term.subterms

	    | _ ->
		false
	end
	&&
        (* clauses are identical *)
	Term.clause_approx_equal clause0 clause1
	  
      let hash (_label, clause) : int =
	Term.hash_of_clause clause		
    end
  )

(* where literal is the label used in the other parts of the clause
   as the connection literal. *)
type labels =
    literal Labels.t



module VarTable = Var.VarTable



(* get the least connected variable in the clause.
   ignores fully connected variables. *)
let get_least_connected (to_split: (literal * var list) list) : var option =
  let shared =
    List.fold_left
      (fun acc (_, vars) ->
	 Tools.lists_merge Var.equal acc vars
	)
      []
      to_split
  in
  let shared_size =
    List.length shared
  in      
    
  (* find out with how many variables a variable is connected,
     i.e. which variable occurs together with the least number of variables
     in all literals. *)
  let connections (*: var list ref VarTable.t*) =
    VarTable.create 64
  in
    List.iter
      (fun (_, vars) ->
	 (* for each variable of this literal... *)
	 List.iter
	   (fun var ->
	      let vars' =
		try
		  VarTable.find connections var
		with
		  | Not_found ->
		      let vars' =
			VarTable.create 64
		      in
			VarTable.add connections var vars';
			vars'
	      in
		(* variable already fully connected *)
		if VarTable.length vars' == shared_size then
		  ()
		    
		else begin
		  (* ... in combination with each variable of this literal *)
		  List.iter
		    (fun var' ->
		       (* ... check if the second one is already registered
			  as shared with the first one. *)
		       (* var already registered as shared *)
		       if VarTable.mem vars' var' then
			 ()
			   
		       (* new shared variable *)
		       else
			 VarTable.add vars' var' ()
		    )
		    vars
		end
	   )
	   vars
      )
      to_split;

    (* find out which variable is connected the least *)
    let best =
      VarTable.fold
	(fun var connected best ->
	   let count =
	     VarTable.length connected
	   in
	     (* variable is fully connected, so not usable for split. *)
	     if count = VarTable.length connections then
	       best
	     else
	       match best with
		 | Some (_, count') when
		     count' <= count ->
		     best
		       
		 | _ ->
		     Some (var, count)
	)
	connections
	None
    in
      match best with
	| None ->
	    None

	| Some (var, _) ->
	    Some var


(* create a fresh connection label for the clause split into (left, left_vars) and right.
   returns
   - the variables shared by the left and right part
   - the label of the left part
   - the label of the right part
*)
let create_connection_label (left: clause) (left_vars: var list) (right: (literal * var list) list) :
    var array * literal * literal =
  let shared_vars =
    List.fold_left
      (fun shared_vars left_var ->
	 (* shared *)
	 if
	   List.exists
	     (fun (_, vars) ->
		List.exists
		  (fun right_var -> Var.equal left_var right_var)
		  vars
	     )
	     right
	 then
	   left_var :: shared_vars
	     
	 (* not shared *)
	 else
	   shared_vars
      )
      []
      left_vars
  in
  let shared_vars' =
    Array.of_list shared_vars
  in
  let shared_size =
    Array.length shared_vars'
  in
  let connection_symbol =
    Symbol.create_connection shared_size
  in
  let connection_terms =
    Array.map Term.request_var shared_vars'
  in
  let connection_term =
    Term.request_func (connection_symbol, connection_terms)
  in
    
  let left_label, right_label =
    let pos_label =
      Term.request_literal true connection_term
    and neg_label =
      Term.request_literal false connection_term
    in
      (* ensure that horn clauses are split into horn clauses *)
      if Term.is_definit left then
	neg_label, pos_label
	  
      (* otherwise, put the negative labels into the connection clause.
	 this turned out to be best. *)
      else			    
	pos_label, neg_label
  in
    shared_vars', left_label, right_label





(* split a clause into two parts, if possible.
   - labels: the already split clause parts with their labels
   - to_split: the clause to split, along with the variables of each literal
   - connection_clause: the connection literals used to split off the other part of to_split
*)
let rec split_clause (labels: labels) (counter: counter) (to_split: (literal * var list) list) : clause list option =
  (* no point in trying to split a unit clause *)
  if List.length to_split < 2 then
    None

  else begin
    match get_least_connected to_split with
      | None ->
	  (* clause is ground or all variables are fully connected. *)
	  None

      | Some least_connected ->
	  Counter.inc counter;

	  (* split the clause according to the least connected variable. *)
	  let rec partition left left_vars right literals =
	    match literals with
	      | [] ->
		  left, left_vars, right

	      | ((literal, vars) as full) :: tail ->
		  if List.exists (fun var -> Var.equal least_connected var) vars then
		    partition (literal :: left) (Tools.lists_merge Var.equal left_vars vars) right tail

		  else
		    partition left left_vars (full :: right) tail
	  in
	  let left, left_vars, right =
	    partition [] [] [] to_split
	  in

	  (* create a fresh connection literal *)
	  let shared_vars, left_label, right_label =
	    create_connection_label left left_vars right
	  in

	  (* normalize left part *)
	  let left_label_normalized, left_clause_normalized =
	    match
	      Subst.normalize_clause (left_label :: Term.sort_clause left)
	    with
	      | head :: tail ->
		  head, tail
		    
	      | _ ->
		  failwith "Preprocessing_split_ground.split_clause normalizing label"
	  in

	    (* retrieve labelled version, if available *)
	    begin
	      try
		let left_label_stored =
		  Labels.find labels (left_label_normalized, left_clause_normalized)
		in

		(* denormalize stored right label,
		   so that its variables match the current one. *)
		let right_label =
		  let term =
		    match left_label.Term.atom, left_label_stored.Term.atom with
		      | Term.Func func, Term.Func func' ->
			  Term.request_func (func'.Term.symbol, func.Term.subterms)
			    
		      | _ ->
			  failwith "Preprocessing_split_ground.denormalize"
		  in
		    Term.request_literal left_label_stored.Term.sign term
		in

		(* add the connection literal to the (not yet normalized) right side *)
		let right =
		  ((right_label, Array.to_list shared_vars) :: right)
		in
		  (* split the right side again *)
		  begin
		    match split_clause labels counter right with
		      | None ->
			  (* no further split, so normalize the right side *)
			  Some [Subst.normalize_clause (Term.sort_clause (List.map fst right))]
			    
		      | Some right' ->
			  Some right'
		  end

		with
		  | Not_found ->
		      (* store as newly labelled *)
		      Labels.add labels
			(left_label_normalized, left_clause_normalized)
			(Term.request_negated_literal left_label_normalized);
		      
		      (* extend left and right side by the new label.
			 only the left side is normalized at this moment. *)
		      let left =
			left_label_normalized :: left_clause_normalized
		      in
		      let right =
			((right_label, Array.to_list shared_vars) :: right)
		      in
			(* split the right side again *)
			begin
			  match split_clause labels counter right with
			    | None ->
				(* no further split, so normalize the right side *)
				Some (left :: [Subst.normalize_clause (Term.sort_clause (List.map fst right))])
				  
			    | Some right' ->
				Some (left :: right')
			end
	      end
  end



let split ~(print:bool) (clauses: clause list) : clause list =
  if print then begin
    print_endline ("Preprocessing: Ground Splitting");
  end;

  let labels =
    Labels.create 64
  in
  let counter =
    Counter.create_with 0
  in

  (* split each clause *)
  let rec split' to_split already_split split_done =
    match to_split with
      | [] ->
	  if print then begin
	    print_endline ("Performed " ^ string_of_int (Counter.value counter) ^ " non-ground splits.");
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
	    let clause' =
	      List.map
		(fun literal ->
		   literal, Term.vars_of_literal literal
		)
		clause
	    in
	      match split_clause labels counter clause' with
		| None ->
		    split' tail (clause :: already_split) split_done

		| Some [] ->
		    failwith "Preprocessing_split_ground.split: empty clause"

		| Some split ->
		    if print then begin
		      print_endline ("Non-ground split: " ^ Term.clause_to_string clause);
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
