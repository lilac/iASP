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


type literal = Term.literal
type clause = Term.clause
type subst = Subst.subst

(* these two clauses resolve to the empty clause *)
exception EMPTY_CLAUSE of (clause * clause)


(* for a clause a mapping from each literal to
   - the literal
   - the clause
   - the clause length *)
class data =
object
  method is_equal (l1, c1, (_: int)) (l2, c2, (_: int)) =
    Term.literal_equal l1 l2 && Term.clause_equal c1 c2

  method to_string (l, c, (_: int)) =
    Term.literal_to_string l ^ " : " ^ Term.clause_to_string c
end




(* compute all resolvents for a clause *)
let rec compute_resolvents_for_clause ~(print:bool)
    (index: (literal * clause * int) Term_indexing.index)
    (resolvents: clause list) (number_of_resolvents: int)
    (clause: clause) :
    (* resolvents * number of resolvents *)
    clause list * int =

  let length =
    List.length clause
  in

  (* create instance of one of the two clauses to resolve.
     literal is the literal resolved on,
     offset the offset of the clause in the resolving unifier. *)
  let rec create_part_resolvent clause literal offset =
    match clause with
      | [] ->
	  []

      | literal' :: tail ->
	  if Term.literal_equal literal literal' then
	    create_part_resolvent tail literal offset

	  else
	    (literal', offset) :: create_part_resolvent tail literal offset
  in

  (* find for each clause literal ... *)
  List.fold_left
    (fun (resolvents, number_of_resolvents) literal ->
      (* max number of resolvents already computed *)
      if number_of_resolvents >= Const.resolvents_max_number then
	(resolvents, number_of_resolvents)

      (* compute next resolvent *)
      else begin
	(* ... find all unifying clauses ... *)
	let unifiable =
	  (index#find (Term.request_negated_literal ~insert_db:false literal))#find_all_unifiable_subst
	    ~p_preserving:false literal.Term.atom
	in
	  (* ... and compute the resolvent *)
	  List.fold_left
	    (fun (resolvents, number_of_resolvents) ((literal', clause', length'), unifier) ->
	      (* max number of resolvents already computed *)
	      if number_of_resolvents >= Const.resolvents_max_number then
		(resolvents, number_of_resolvents)

	      (* eager filter: ignore if (without factorization) resolvent is too long *)
	      else if length + length' > Const.resolvent_max_size + 2 then
		(resolvents, number_of_resolvents)
		  
	      (* create resolvent *)
	      else begin
		let left =
		  create_part_resolvent clause literal Term_indexing.query_offset
		and right =
		  create_part_resolvent clause' literal' Term_indexing.index_offset
		in
		let resolvent =
		  Subst.apply_to_literals unifier (left @ right)
		in

		(* simplify resolvent *)
		let resolvent =
		  Term.remove_duplicates resolvent
		in

		(* drop resolvent if it is too long. *)
		if List.length resolvent > Const.resolvent_max_size then
		  (resolvents, number_of_resolvents)
	
		(* drop resolvent if it is a tautology. *)
		else if Term.is_tautology resolvent then
		  (resolvents, number_of_resolvents)

		(* drop if resolvent already exists *)
		else if List.exists (fun clause' -> Term.clause_equal resolvent clause') resolvents then
		  (resolvents, number_of_resolvents)

		else begin
		  (* normalize resolvent *)
		  let resolvent =
		    Subst.normalize_clause (Term.sort_clause resolvent)
		  in

		  if print then begin
		    print_endline ("RESOLVENT: " ^ Term.clause_to_string resolvent);
		    print_endline (Term.literal_to_string literal ^ "-->: " ^ Term.clause_to_string clause);
		    print_endline (Term.literal_to_string literal' ^ "-->: " ^ Term.clause_to_string clause');
		    print_endline (Subst.subst_to_string unifier);
		    print_newline ();
		  end;

		  (* resolved to empty clause? *)
		  begin
		    match resolvent with
		      | [] ->
			  raise (EMPTY_CLAUSE (clause, clause'))
			    
		      | _ ->
			  (resolvent :: resolvents, number_of_resolvents + 1)
		  end;
		end
	      end
	    )
	    (resolvents, number_of_resolvents)
	    unifiable
	end
    )
    (resolvents, number_of_resolvents)
    clause



(* compute all resolvents between the clauses *)
let compute_resolvents ~(print:bool) (clauses: clause list) : clause list =
  if print then begin
    print_endline ("Preprocessing: Computing resolvents of size <= " ^ string_of_int Const.resolvent_max_size);
  end;

  (* register all literals of clauses with length <= max_size + 1 -
     to speed up computation we ignore the others right away. *)
  let index =
    Discrimination_tree.create_index false (new data)
  in
    List.iter
      (fun clause ->
	let length =
	  List.length clause
	in
	  if length <= Const.resolvent_max_size + 1 then begin
	    List.iter
	      (fun literal ->
		(index#find literal)#add literal.Term.atom (literal, clause, length)
	    ) 
	    clause
	  end;
      )
      clauses;
  
  (* compute resolvents for each clause *)
  let resolvents, _ =
    List.fold_left
      (fun (resolvents, number_of_resolvents) clause ->
	if number_of_resolvents >= Const.resolvents_max_number then
	  (resolvents, number_of_resolvents)

	else if List.length clause <= Const.resolvent_max_size + 1 then
	  compute_resolvents_for_clause ~print:print
	    index resolvents number_of_resolvents clause

	else
	  (resolvents, number_of_resolvents)
      )
      ([], 0)
      clauses
  in

  (* drop resolvent if it already exists as a clause *)
  let resolvents =
    List.find_all
      (fun resolvent ->
	List.for_all
	  (fun clause -> not (Term.clause_equal resolvent clause))
	  clauses
      )
      resolvents
  in

  (* keep only the shortest resolvent *)
  let resolvents =
    Tools.list_first Const.resolvents_max_number
      (
	List.sort
	  (fun x y ->
	    let length =
	      Tools.compare_int (List.length x) (List.length y)
	    in
	      if length != 0 then
		length
	      else
		Tools.compare_int (Term_attributes.weight_of_clause x) (Term_attributes.weight_of_clause y)
	  )
	  resolvents
      )
  in

    (* resolve resolvents *)
  let resolvents', _ =
    List.fold_left
      (fun (resolvents, number_of_resolvents) clause ->
	if number_of_resolvents >= Const.resolvents_max_number then
	  (resolvents, number_of_resolvents)

	else if List.length clause <= Const.resolvent_max_size + 1 then
	  compute_resolvents_for_clause ~print:print
	    index resolvents number_of_resolvents clause

	else
	  (resolvents, number_of_resolvents)
      )
      ([], 0)
      resolvents
  in
  let resolvents' =
    List.find_all
      (fun resolvent ->
	List.for_all
	  (fun clause -> not (Term.clause_equal resolvent clause))
	  clauses
      )
      resolvents'
  in
  let resolvents' =
    List.find_all
      (fun resolvent ->
	List.for_all
	  (fun clause -> not (Term.clause_equal resolvent clause))
	  resolvents
      )
      resolvents'
  in
  let resolvents =
    Tools.list_first Const.resolvents_max_number
      (
	List.sort
	  (fun x y ->
	    let length =
	      Tools.compare_int (List.length x) (List.length y)
	    in
	      if length != 0 then
		length
	      else
		Tools.compare_int (Term_attributes.weight_of_clause x) (Term_attributes.weight_of_clause y)
	  )
	  resolvents' @ resolvents
      )
  in

    if print then begin
      print_newline ();
    end;
    resolvents
