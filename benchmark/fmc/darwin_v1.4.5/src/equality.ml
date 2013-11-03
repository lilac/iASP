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



type var = Var.var
type symbol = Symbol.symbol
type term = Term.term
type clause = Term.clause


    

(* basic equality axioms *)
let get_equality_clauses () : clause list =
  [
    (* Reflexivity *)
    Read_tme.to_clause "(X = X).";
    (* Symmetry *)
    Read_tme.to_clause "(Y = X) :- (X = Y).";
    (* Transitivity *)
    Read_tme.to_clause "(X = Z) :- (X = Y), (Y = Z).";
  ]





(* creates for a symbol s permutations of the terms
   s(_0, _1, _2)
   s(_3, _1, _2)
   (_0 = _3)
   where _3 shifts over all positions.
   applies assemble to it to build a clause.
 *)
let do_symbol (symbol: symbol) (acc: clause list)
    (assemble: term -> term -> term -> clause) : clause list =

  let int_to_term (i: int) : term =
    Term.request_var (Var.create_universal i)
  in

  let arity =
    Symbol.arity symbol
  in
  let schema =
    Array.make arity 0
  in
    for i = 0 to Array.length schema - 1 do
      schema.(i) <- i;
    done;
    
    (* a list of 0 ... arity - 1 *)
    let rec do_at (index: int) (acc: clause list) : clause list =
      if index >= arity then
	acc
	    
      else begin
	let left_term =
	  Term.request_func (symbol, Array.map int_to_term schema)
	in

	(* temporarily replace the index variable *)
	schema.(index) <- arity;
	let right_term =
	  Term.request_func (symbol, Array.map int_to_term schema)
	in
	  schema.(index) <- index;
	let equality_term =
	  Term.request_func
	    (Symbol.equality, [| int_to_term index; int_to_term arity |])
	in
	let clause =
	  assemble left_term right_term equality_term
	in
	  do_at (index + 1) (clause :: acc)
      end
    in
      do_at 0 acc


(*
  the substitution axioms for the predicate p(X, Y, Z) are:
  
  p(_0, _1, _2) :- p(_3, _1, _2)), (_0 = _3).
  p(_0, _1, _2) :- p(_0, _3, _2)), (_0 = _3).
  p(_0, _1, _2) :- p(_0, _1, _3)), (_0 = _3).
*)
let get_predicate_substitutions (symbols: symbol list) : clause list =

  let assemble left right equal =
    let left_predicate =
      Term.request_literal true left
    and right_predicate =
      Term.request_literal false right
    and equality_predicate =
      Term.request_literal false equal
    in
      [ left_predicate; right_predicate; equality_predicate ]
  in

    List.fold_left
      (fun acc symbol ->
	 do_symbol symbol acc assemble
      )
      []
      symbols


(*
  the substitution axioms for the function f(X, Y, Z) are:

  (f(_0, _1, _2) = f(_3, _1, _2)) :- (_0 = _3).
  (f(_0, _1, _2) = f(_0, _3, _2)) :- (_1 = _3).
  (f(_0, _1, _2) = f(_0, _1, _3)) :- (_2 = _3).
*)
let get_function_substitutions (symbols : symbol list) : clause list =

  let assemble left right equal =
    let left_equality_term =
      Term.request_func (Symbol.equality, [| left; right |])
    in
    let left_equality_predicate =
      Term.request_literal true left_equality_term
    in
    let right_equality_predicate =
      Term.request_literal false equal
    in
      [ left_equality_predicate; right_equality_predicate ]
  in

    List.fold_left
      (fun acc symbol ->
	 do_symbol symbol acc assemble
      )
      []
      symbols




let get_axioms ~(print_axioms: bool) (problem: Problem.problem) : clause list =
  let predicate_symbols, function_symbols =
    problem#getPredicateSymbols, problem#getFunctionSymbols
  in

  let axioms =
    get_equality_clauses ()
  and predicate_substitutions =
    get_predicate_substitutions predicate_symbols
  and function_substitutions =
    get_function_substitutions function_symbols
  in

    if print_axioms then begin
      print_endline ("Equality Axioms:");
      List.iter (fun clause -> print_endline (Term.clause_to_string clause)) axioms;
      print_newline ();
      
      if List.length predicate_substitutions > 0 then begin
	print_endline ("Predicate Substitution Axioms:");
	List.iter (fun clause -> print_endline (Term.clause_to_string clause)) predicate_substitutions;
	print_newline ();
      end;

      if List.length function_substitutions > 0 then begin
	print_endline ("Function Substitution Axioms:");
	List.iter (fun clause -> print_endline (Term.clause_to_string clause)) function_substitutions;
	print_newline ();
      end;
      print_newline ();
    end;

    axioms @ predicate_substitutions @ function_substitutions
