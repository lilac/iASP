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



type term = Term.term
type literal = Term.literal
type raw_context_unifier = Context_unifier.raw_context_unifier


class context_unifier_data =
object
  method is_equal (first: raw_context_unifier) (second: raw_context_unifier) : bool =
    Context_unifier.compare_context_unifiers ~different:false first second = 0
      
  method to_string (data: raw_context_unifier) : string =
    Context_unifier.raw_context_unifier_to_string data
end

type predicate_index = raw_context_unifier Term_indexing.predicate_index
type index = raw_context_unifier Term_indexing.index

type selection_lookahead = {
  (* the maximal number of candidates to store for each (predicate * sign) *)
  sl_limit: int;
  mutable sl_size: int;
  sl_candidates: index;
}
    




(*** functions ***)


let create (limit: int) : selection_lookahead =
  let candidates =
    Discrimination_tree.create_index false (new context_unifier_data)
(*    Substitution_tree.create_index (new context_unifier_data)*)
  in
    {
      sl_limit = limit;
      sl_size = 0;
      sl_candidates = candidates;
    }

let is_full selection_lookahead =
  selection_lookahead.sl_size >= selection_lookahead.sl_limit


let add ~(no_duplicates: bool) (selection_lookahead: selection_lookahead)
  (literal: literal) (raw_context_unifier: raw_context_unifier) : unit =

  if not (is_full selection_lookahead) then begin
    let literal =
      Term.insert_literal literal
    in
    let index =
      selection_lookahead.sl_candidates#find literal
    in
      (* we could also do a p-preserving subsumption test,
	 but that's too expensive. *)
      index#add ~no_duplicates:no_duplicates literal.Term.atom raw_context_unifier;
      selection_lookahead.sl_size <- selection_lookahead.sl_size + 1;
    end


let check (selection_lookahead: selection_lookahead) (literal: literal) : bool =
  let index =
    selection_lookahead.sl_candidates#find (Term.request_negated_literal ~insert_db:false literal)
  in
    match index#find_unifiable ~p_preserving:true literal.Term.atom with
      | None ->
	  false
	    
      | Some _ ->
	  true



let backtrack (selection_lookahead: selection_lookahead) : unit =
  (* for each index... *)
  selection_lookahead.sl_candidates#iter
    (fun _ index ->
       (* ... find all invalid entries *)
       let remove_entries =
	 index#fold
	   (fun acc term raw_context_unifier ->
	      if Context_unifier.is_raw_context_unifier_invalid raw_context_unifier then
		(term, raw_context_unifier) :: acc
	      else
		acc
	   )
	   []
       in
	 (* ... and remove them *)
	 List.iter
	   (fun (term, raw_context_unifier) ->
	      if not (index#remove term (Some raw_context_unifier)) then begin
		print_endline (Term.term_to_string term);
		print_endline (Context_unifier.raw_context_unifier_to_string raw_context_unifier);
		failwith "Selection_lookahead.backtrack"
	      end;
	      selection_lookahead.sl_size <- selection_lookahead.sl_size - 1;
	   )
	   remove_entries
    )
