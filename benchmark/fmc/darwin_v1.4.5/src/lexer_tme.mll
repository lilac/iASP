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



(* A lexer for a subset of the protein tme format for first order clauses *)

{ 

open Parser_tme

let prev_column_index =
  ref 1

let current_column_index =
  ref 1

let prev_line_index =
  ref 1

let current_line_index =
  ref 1

let current_token =
  ref ""

let update_column_index (value: int) =
  prev_column_index := !current_column_index;
  current_column_index := value


let update_line_index () =
  prev_line_index := !current_line_index;
  current_line_index := !current_line_index + 1;
  update_column_index 1


let count_new_lines (string: string) : unit =
  String.iter
    (fun char ->
       if char = '\n' then
	 update_line_index ()
       else
	 update_column_index (!current_column_index + 1)
    )
    string

let update_token (token: string) =
  current_token := token;
  update_column_index (!current_column_index + String.length token)


let lexing_error (error: string) (token: string) =
  print_endline (error
	    ^ " at line " ^ string_of_int !prev_line_index
	    ^ " column " ^ string_of_int !prev_column_index
	    ^ ":\n" ^ token);
  raise Const.PARSE_ERROR

let parse_error () =
  print_endline ("Parse error"
	    ^ " at line " ^ string_of_int !prev_line_index
	    ^ " column " ^ string_of_int !prev_column_index
	    ^ ":\n" ^ !current_token);
  raise Const.PARSE_ERROR

}




(* regular expressions *)

let one_line_comment =
  '%' [^ '\n' '\r']* ('\n' | "\r\n")

let multi_line_comment =
  "/*" ( [^ '*'] | ('*' [^ '/']) )* "*/"

let multi_line_comment_unclosed =
  "/*" ( [^ '*'] | ('*' [^ '/']) )* eof

let variable =
  [ 'A'-'Z' '_' ] [ 'a'-'z' 'A'-'Z' '0'-'9' '_']*
  
let symbol =
  (*[ 'a'-'z' '0'-'9' ':' '*' '+' '-' '/'] [ 'a'-'z' 'A'-'Z' '0'-'9' '_']* *)
  [ 'a'-'z' '0'-'9' '_'] [ 'a'-'z' 'A'-'Z' '0'-'9' '_']*

let implication =
  ":-" | "<-"

let negation =
  "-" | "~"





rule token =
    parse
      | [' ' '\t' '\r']              { update_token (Lexing.lexeme lexbuf);
				       token lexbuf } (* skip blanks *)
      | ['\n']                       { update_line_index ();
				       current_token := Lexing.lexeme lexbuf;
				       token lexbuf } (* skip new lines *)
      | one_line_comment             { update_line_index ();
				       current_token := Lexing.lexeme lexbuf;
				       token lexbuf } (* skip comment *)
      | multi_line_comment           { count_new_lines (Lexing.lexeme lexbuf);
				       current_token := Lexing.lexeme lexbuf;
				       token lexbuf } (* skip comment *)
      | multi_line_comment_unclosed  { prev_column_index := !current_column_index;
				       prev_line_index := !current_line_index;
				       lexing_error "Unclosed Comment" (Lexing.lexeme lexbuf) }
	  (* end of input - for channels, strings, ... *)
      | eof                          { update_token (Lexing.lexeme lexbuf); EOI }
      | "true"                       { update_token (Lexing.lexeme lexbuf); TRUE }
      | "false"                      { update_token (Lexing.lexeme lexbuf); FALSE }
      | symbol                       { update_token (Lexing.lexeme lexbuf); SYMBOL (Lexing.lexeme lexbuf) }
      | variable                     { update_token (Lexing.lexeme lexbuf); VARIABLE (Lexing.lexeme lexbuf) }
      | '('                          { update_token (Lexing.lexeme lexbuf); LEFT_PARENTHESIS }
      | ')'                          { update_token (Lexing.lexeme lexbuf); RIGHT_PARENTHESIS }
      | '{'                          { update_token (Lexing.lexeme lexbuf); LEFT_BRACE }
      | '}'                          { update_token (Lexing.lexeme lexbuf); RIGHT_BRACE }
      | implication                  { update_token (Lexing.lexeme lexbuf); IMPLICATION }
      | negation                     { update_token (Lexing.lexeme lexbuf); NEGATION }
      | '.'                          { update_token (Lexing.lexeme lexbuf); CLAUSE_END }
      | ';'                          { update_token (Lexing.lexeme lexbuf); SEMICOLON }
      | ","                          { update_token (Lexing.lexeme lexbuf); COMMA }
      | '='                          { update_token (Lexing.lexeme lexbuf); EQUALITY }
      | _                            { prev_column_index := !current_column_index;
				       prev_line_index := !current_line_index;
				       lexing_error "Invalid Input" (Lexing.lexeme lexbuf) }


{ 

(* footer *)

}
