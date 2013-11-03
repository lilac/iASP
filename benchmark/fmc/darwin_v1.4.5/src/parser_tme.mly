/*
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
*/


%{
  
  type var = Var.var
  type symbol = Symbol.symbol
  type term = Term.term
  type literal = Term.literal
  type clause = Term.clause

  (* these are only valid for the current clause
     and have to be invalidated with init_clause for every new clause *)

  (* the variable id counter for the currently read term/clause *)
  let var_id_counter = 
    Counter.create_with 0
      
  (* mapping of the variable names (e.g. "X") of the currently read term/clause
     to variables. *)
  let (var_map: (string * Var.var) list ref) =
    ref []

  (* the literals of the currently read clause *)
  let (literals: literal list ref) =
    ref []


  (* reset everything in order to parse a new term/clause *)
  let init_clause () =
    Counter.set var_id_counter 0;
    var_map := [];
    literals := []


	
  (* gets the variables associated with a string from the variable mapping
     creates a new mapping for a new variable *)
  let get_var (var_name: string) =
    try 
      (* way faster than List.assoc *)
      snd (
	List.find
	  (fun (var_name', _) ->
             var_name = var_name'
	  )
	  !var_map
      )
    with
      | Not_found ->
	  let new_var = 
	    Var.create_universal (Counter.value var_id_counter)
	  in
	    Counter.inc var_id_counter;
	    var_map := (var_name, new_var) :: !var_map;
	    new_var
	      

%}
  
%token <string> SYMBOL // a function or predicate symbol
%token <string> VARIABLE
%token LEFT_PARENTHESIS
%token RIGHT_PARENTHESIS
%token LEFT_BRACE
%token RIGHT_BRACE
%token IMPLICATION
%token CLAUSE_END
%token NEGATION
%token TRUE
%token FALSE
%token SEMICOLON
%token COMMA
%token EQUALITY
%token EOI

%start parse_clause
%type <Term.clause> parse_clause
%start parse_file
%type <Term.clause list> parse_file


%%

/* start rules */

parse_file:
  | file EOI 
      { $1 }

  | EOI
      { print_endline "empty problem specification"; raise Const.PARSE_ERROR }

parse_clause:
  | clause EOI
      { $1 }

  | EOI
      { [] }

/* parse rules */



file:
  | clause
      { [$1] }

  | clause file
      { $1 :: $2 }

clause: /* { p(a), -q(f(V)) } */
  | clause_head CLAUSE_END
      {
	let clause = 
	  $1
	in
	  init_clause ();
	  clause
      }

  | clause_head IMPLICATION clause_body CLAUSE_END 
      {
	let clause = 
	  List.append $1 $3
	in
	  init_clause ();
	  clause
      }

clause_head: /* p(a) */
  | literal
      { (* remove leading false *)
	if Term.literal_equal Term.false_literal $1 then
	  []
	else
	  [$1] 
      }

  | literal SEMICOLON clause_head
      { $1 :: $3}

  | literal COMMA clause_head
      { $1 :: $3}

clause_body: /* q(f(V)); q(X) */
  | literal
      { (* remove true body *)
	if Term.literal_equal Term.true_literal $1 then
	  []
	else
	  [ Term.request_negated_literal $1 ] 
      }

  | literal COMMA clause_body
      { Term.request_negated_literal $1 :: $3 }

literal: /* -q(f(V)) */
  | TRUE
      { print_endline "!!TRUE"; Term.true_literal }

  | FALSE
      { Term.false_literal }

  | NEGATION literal
      {	Term.request_negated_literal $2 }

  | term
      { Term.request_literal true $1 }

  | LEFT_PARENTHESIS sub_term_list infix sub_term_list RIGHT_PARENTHESIS 
      { Term.request_literal true
	  (Term.request_func (Symbol.create_predicate $3 2, Array.of_list ($2 @ $4)))
      }

infix: /* infix predicate symbols */
  | EQUALITY 
      { "=" }


term: /* f(X) */
  | top_func
      { $1 }
/*  | var
      { Term.request_var $1 }*/
  | SYMBOL
      { Term.request_const (Symbol.create_predicate $1 0) }


/* top term of a predicate: p(_) */
top_func:
  | constant LEFT_PARENTHESIS sub_term_list RIGHT_PARENTHESIS 
      { Term.request_func (Symbol.create_predicate $1 (List.length $3), Array.of_list $3) }

/* subterm of a term: p(f(_)) */
func:
  | constant LEFT_PARENTHESIS sub_term_list RIGHT_PARENTHESIS 
      { Term.request_func (Symbol.create_function $1 (List.length $3), Array.of_list $3) }

sub_term_list:
  | sub_term COMMA sub_term_list
      { $1 :: $3 }
  | sub_term
      { [$1] }

sub_term:
  | func
      { $1 }
  | var
      { Term.request_var $1 }
  | constant
      { Term.request_const (Symbol.create_function $1 0)  }

var: /* X */
  | VARIABLE
      { get_var $1 }

constant: /* a */
  | SYMBOL
      { $1 }

%%
