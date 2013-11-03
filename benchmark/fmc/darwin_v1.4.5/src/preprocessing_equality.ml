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

exception EMPTY_CLAUSE of clause




(*
  Disequalities

  1) -(x = t) \/ C -> C{x -> t} where t a term or variable and x not in t

  2) -(t = t) \/ C -> C where t a term or variable
*)
let rec apply_disequality (clause: clause) =

  (* try to find a disequality in to_check and apply it *)
  let rec apply_disequality' (to_check: clause) : clause * clause =
    match to_check with
      | [] ->
	  (* no more suitable disequality found *)
	  begin
	    match clause with
	      | [] ->
		  (* clause has been simplified to the empty clause
		     because no disequality was satisfiable *)
		  raise (EMPTY_CLAUSE clause)

	      | _ ->
		  clause, []
	  end;
	    
      | literal :: tail ->
	  (* skip - not a negative disequality *)
	  if literal.Term.sign then
	    apply_disequality' tail
	      
	  else begin
	    match literal.Term.atom with
	      | Term.Func func when
		  Symbol.equal Symbol.equality func.Term.symbol ->

		  (* drop the literal: -(t = t) *)
		  if Term.term_equal func.Term.subterms.(0) func.Term.subterms.(1) then begin
		    let clause' =
		      List.find_all
			(fun literal' -> not (Term.literal_equal literal literal'))
			clause
		    in
		    let clause'', simplified = apply_disequality clause' in
		      clause'', (literal :: simplified)
		  end

		  (* a suitable disequality: -(x = t) *)
		  else begin
		    match func.Term.subterms.(0), func.Term.subterms.(1) with
		      | Term.Var var, term
		      | term, Term.Var var ->

			  (* keep the literal: -(x = t(x)) *)
			  if Term.term_contains_var term var then begin
			    apply_disequality' tail
			  end

			  (* try to apply the disequality *)
			  else begin
			    let clause'', simplified =
			      apply_disequality'' clause literal var term
			    in
			    let clause''', simplified' =
			      apply_disequality clause''
			    in
			      clause''', simplified' @ simplified
			  end
			    
		      | _ ->
			  (* skip - an inequality between non-variable terms *)
			  apply_disequality' tail
		  end
		    
	      | _ ->
		  (* skip - not a disequality *)
		  apply_disequality' tail
	end
  in
    apply_disequality' clause


(* apply the literal -(var = term) to the clause *)
and apply_disequality'' clause literal var term : clause * clause =
  match clause with
    | [] ->
	[], []
	  
    | literal' :: tail ->
	(* skip the disequality to remove *)
	if Term.literal_equal literal literal' then
	  let tail', simplified = apply_disequality'' tail literal var term in
	    tail', literal' :: simplified
	    
	(* optimization: don't create a new term
	   if the variable to replace does not occur in it *)
	else if not (Term.literal_contains_var literal' var) then
	  let tail', simplified = apply_disequality'' tail literal var term in
	    literal' :: tail', simplified
	    
	(* apply disequality *)
	else
	  let literal'' =
	    Term.replace_vars_in_literal
	      literal'
	      (fun var' term' ->
		if Var.equal var var' then
		  term
		else
		  term'
	      )
	  in
	  let tail', simplified = apply_disequality'' tail literal var term in
	    literal'' :: tail', simplified



let rec simplify' ~(print: bool) (clauses: clause list) : clause list * clause list =
  match clauses with
    | [] ->
	[], []
	  
    | clause :: tail ->
	begin
	  try
	    (* inline disequalities *)
	    let clause', simplified =
	      apply_disequality clause
	    in
	    let tail', simplified' =
	      (* remove tautologies *)
	      if Term.is_tautology clause' then begin
		if print then begin
		  print_endline ("Tautology: " ^ Term.clause_to_string clause);
		  (* print simplification, if done *)
		  if clause != clause' then
		    print_endline ("--> : " ^ Term.clause_to_string clause');

		  print_newline ();
		end;

		simplify' ~print:print tail
	      end

	      (* no simplification, keep original clause *)
	      else if clause == clause' then begin
		let clauses', simplified = simplify' ~print:print tail in
		  clause :: clauses', simplified
              end

	      (* keep simplified clause *)
	      else begin
		if print then begin
		  print_endline ("Transform: " ^ Term.clause_to_string clause);
		  print_endline ("---> " ^ Term.clause_to_string clause');
		  print_newline ();
		end;

		let clauses', simplified = simplify' ~print:print tail in
		  (Subst.normalize_clause (Term.sort_clause clause')) :: clauses', simplified
	      end;
	    in
	      match simplified with
		| [] -> tail', simplified'
		| _  -> tail', simplified :: simplified'
	    with
	      | EMPTY_CLAUSE _ ->
		  raise (EMPTY_CLAUSE clause)
	  end


let simplify ~(print: bool) (clauses: clause list) : clause list * clause list =
  if print then begin
    print_endline ("Preprocessing: Ground Splitting");
  end;

  let clauses', simplified =
    simplify' ~print:print clauses
  in

  if print then begin
    print_newline ();
  end;

    clauses', simplified
