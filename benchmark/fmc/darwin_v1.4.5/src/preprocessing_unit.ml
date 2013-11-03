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


exception EMPTY_CLAUSE of clause



(*** functions ***)


(* for each clause do unit subsumption / resolution *)
let rec simplify' ~(print:bool)
  (* processed clauses *)
  (processed: clause list)
  (* clauses to simplify *)
  (clauses: clause list)
  (* index of unit clauses to use for simplification right now. *)
  (index : literal Term_indexing.index)
  (* clauses and literals removed by simplification *)
  (simplified: clause list)
  :
  (* processed * simplified *)
  (clause list) * (clause list) =

  match clauses with
    | [] ->
	(* *)
	processed, simplified

    | clause :: tail ->
	begin
	  match clause with
	    | _ :: [] ->
		(* just keep any unit clause.
		   if several unit clauses subsume each other we ignore this,
		   this is efficiently handled by Assert anyway.
		   otherwise, if the same literal exists several times,
		   we would make sure to keep at least one. *)
		simplify' ~print:print (clause :: processed) tail index simplified

	    | _ ->
		begin
		  (* subsumed by a unit clause? *)
		  if
		    List.exists
		      (fun literal ->
			match (index#find literal)#find_generalization ~p_preserving:false literal.Term.atom with
			  | None -> false
			  | Some _ ->
			      if print then begin
				print_endline ("Subsumed: " ^ Term.literal_to_string literal ^ " --> "
					      ^ Term.clause_to_string clause);
			      end;
			      true
		      )
		      clause
		  then begin
		    (* subsuming unit clause exists. *)
		    simplify' ~print:print processed tail index (clause :: simplified)
		  end

		  (* try to resolve *)
		  else begin
  		    (* find resolving unit clauses for each clause literal,
		       returns the (simplified) clause, and which literals were removed. *)
		    let rec resolve (processed: literal list) (to_simplify: literal list)
			(simplified: literal list) : (literal list) * (literal list) =
		      match to_simplify with
			| [] ->
			    if simplified <> [] then begin
 			      if print then begin
				print_endline (Term.clause_to_string clause ^ " --> " ^ Term.clause_to_string processed);
			      end;
			      processed, simplified
			    end
			    else
			      clause, simplified

			| literal :: tail ->
			    begin
			      let resolving_clause =
				(index#find (Term.request_negated_literal ~insert_db:false literal))#find_generalization
                                  ~p_preserving:false literal.Term.atom
                              in
				match resolving_clause with
				  | None ->
				      resolve (literal :: processed) tail simplified

				  | Some resolving_literal ->
				      if print then begin
					print_endline ("Resolved: " ^ Term.literal_to_string resolving_literal
					^ " in " ^ Term.clause_to_string clause);
				      end;
				      resolve processed tail (literal :: simplified)
			    end
		    in
		    let clause', simplified' =
		      resolve [] clause []
		    in
		      begin
			match clause' with
			  | [] ->
			      raise (EMPTY_CLAUSE clause)
				
			  | literal :: [] ->
			      (* new unit clause *)
			      (index#find literal)#add literal.Term.atom literal;
			  | _ ->
			      ()
		      end;
		      let simplified'' =
			match simplified' with
			  | [] -> simplified
			  | _  -> simplified' :: simplified
		      in		      
			simplify' ~print:print (clause' :: processed) tail index simplified''
		    end
		end
	end




let simplify ~(print: bool) (clauses: clause list) : (clause list * clause list) =
  if print then begin
    print_endline ("Preprocessing: Unit");
  end;

  (* first build index of unit clauses *)
  let index =
    Discrimination_tree.create_literal_index false
  in
    List.iter
      (fun clause ->
	 match clause with
	   | literal :: [] ->
	       (index#find literal)#add literal.Term.atom literal

	   | _ ->
	       ()
      )
      clauses;

  (* simplify *)
  let clauses', simplified =
    simplify' ~print:print [] clauses index []
  in
    if print then begin
      print_newline ();
    end;

  (* get original order of clauses back *)
  match simplified with
    | [] -> (clauses, [])
    | _  -> (List.rev clauses', simplified)
