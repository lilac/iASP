(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2004  The University of Iowa
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
type term = Term.term
type literal = Term.literal
type clause = Term.clause




let parse_string 
  (parse_function: (Lexing.lexbuf -> Parser_darwin.token) -> Lexing.lexbuf -> 'a)
  (term_type: string)
  (string: string) =
  try
    parse_function Lexer_darwin.token (Lexing.from_string string)
  with
    | Parsing.Parse_error ->
	failwith ("Failed to parse as a " ^ term_type ^ " in format darwin:\n" ^ string)

let parse_from_string (source: string) =
  let
    lexbuf = Lexing.from_string source
  in
    Parser_darwin.parse_file Lexer_darwin.token lexbuf

let parse_from_file (file_name: string) =
  let
    lexbuf = Lexing.from_channel (open_in file_name)
  in
    Parser_darwin.parse_file Lexer_darwin.token lexbuf

let to_var (string: string) : var =
  parse_string Parser_darwin.parse_var "variable" string

let to_term (string: string) : term =
  parse_string Parser_darwin.parse_term "term" string

let to_literal (string: string) : literal =
  parse_string Parser_darwin.parse_literal "literal" string

let to_clause (string: string) : clause =
  parse_string Parser_darwin.parse_clause "clause" string

let to_clauses (string: string) : clause list =
  parse_string Parser_darwin.parse_clauses "clauses" string

let to_clauses_from_string (string: string) : clause list =
  parse_from_string string

let to_clauses_from_file (string: string) : clause list =
  parse_from_file string
