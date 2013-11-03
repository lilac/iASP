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



(*** types ***)

type literal = Term.literal
type clause = Term.clause



(*** functions ***)


(* remove pure clauses.

   returns remaining clauses and pure literals.

   if [fix] is given purification is repeated until a fixpoint is reached. *)
let purify ~(print: bool) ~(fix: bool)
    (is_pure: clause -> literal -> bool) (clauses: clause list)
    : clause list * clause list * literal list =
  
  let rec purify' (purified: clause list)
      (pure: literal list) (clauses: clause list) (simplified: clause list) (fix_reached: bool)
      : clause list * clause list * literal list =
    match clauses with
      | [] ->
	  if fix && not fix_reached then
	    purify' [] pure (List.rev purified) simplified true

	  else
	    List.rev purified, simplified, List.rev pure

      | clause :: tail ->
	  begin
	    try
	      let pure_literal =
		List.find (fun literal -> is_pure clause literal) clause
	      in
		if print then begin
		  print_endline ("Pure: " ^ Term.literal_to_string pure_literal ^ " --> " ^ Term.clause_to_string clause);
		end;
		purify' purified (pure_literal :: pure) tail (clause :: simplified) false
	    with
	      | Not_found ->
		  purify' (clause :: purified) pure tail simplified fix_reached
	  end
  in
    purify' [] [] clauses [] true



let simplify ~(print:bool) ~(equality:bool) ~(finite_domain:bool) (clauses: clause list)
    : clause list * clause list * literal list =
  if print then begin
    print_endline ("Preprocessing: Pure Literals");
  end;

  (* for equality we can not go by unification,
     or perhaps somewhat if we take sorts (Sort_inference) into account.
     currently pure is detected by predicate symbol polarity only.
  *)
  if equality || finite_domain then begin
    (* keep from each predicate symbol a mapping index -> contained in clause *)
    let index =
      Term.LiteralTypeTable.create 1024
    in

    (* now register all clauses *)
    List.iter
      (fun clause ->
	List.iter
	  (fun literal ->
	    let entry =
	      try
		Term.LiteralTypeTable.find index literal
	      with
		| Not_found ->
		    let new_entry =
		      ref []
		    in
		      Term.LiteralTypeTable.add index literal new_entry;
		      new_entry
	    in
	      entry := clause :: !entry;
	  ) 
	  clause
      )
      clauses;

    (* all unifiable literals must be from the clause itself *)
    let is_pure (clause: clause) (literal: literal) : bool =
      (* equalities are never pure *)
      match literal.Term.atom with
	| Term.Func func when Symbol.equal Symbol.equality func.Term.symbol ->
	    false

	| _ ->
	    begin
	      try
		let matching =
		  Term.LiteralTypeTable.find index (Term.request_negated_literal literal)
		in
		  (* pure if all literals of opposite sign are from the same clause *)
 		  List.for_all
		    (fun clause' -> clause == clause')
		    !matching
	      with
		| Not_found ->
		    (* no literal of opposite sign, so pure *)
		    true
	    end
    in

    (* now check for each clause if one of its literals is pure,
       i.e. its negation is not unifiable with a literal from another clause. *)
    let purified, simplified, pure_literals =
      purify ~print:print ~fix:true is_pure clauses
    in

    (* in finite domain mode replace all non-equality literals by schema terms,
       i.e. make all instance true instead of just the pure ones.
    *)
    let pure_literals =
      if not finite_domain then
	pure_literals
      else
	List.map
	  (fun literal ->
	     match literal.Term.atom with
	       | Term.Func func when Symbol.equal Symbol.equality func.Term.symbol ->
		   literal

	       | _ ->
		   Term.request_literal
		     literal.Term.sign
		     (Term.create_schema_term_from_term literal.Term.atom)
	  )
	  pure_literals
    in
      if print then begin
	print_newline ();
      end;
      purified, simplified, pure_literals
  end

  (* purity check via unifying literals *)
  else begin
    (* keep from each literal a mapping index -> contained in claused *)
    let index =
      Discrimination_tree.create_clause_index false
    in

    (* now register all clauses *)
    List.iter
      (fun clause ->
	List.iter
	  (fun literal ->
	    (index#find literal)#add literal.Term.atom clause
	  ) 
	  clause
	)
      clauses;

    (* all unifiable literals must be from the clause itself *)
    let is_pure (clause: clause) (literal: literal) : bool =
      let unifiable =
	(index#find (Term.request_negated_literal ~insert_db:false literal))#find_all_unifiable
	  ~p_preserving:false literal.Term.atom
      in
	List.for_all
	  (fun clause' -> clause == clause')
	  unifiable
    in

    (* now check for each clause if one of its literals is pure,
       i.e. its negation is not unifiable with a literal from another clause. *)
    let purified, simplified, pure_literals =
      purify ~print:print ~fix:false is_pure clauses
    in
      if print then begin
	print_newline ();
      end;
      purified, simplified, pure_literals
  end
