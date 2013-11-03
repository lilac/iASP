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

open Parser_darwin

let line_counter =
  Counter.create_with 1

let count_new_lines (string: string) : unit =
  String.iter
    (fun char ->
       if char = '\n' then
	 Counter.inc line_counter;
    )
    string

let tokenize_error (error: string) (input: string) =
  print_endline (error
	    ^ " at line " ^ string_of_int (Counter.value line_counter)
	    ^ ":\n" ^ input);
  raise Const.PARSE_ERROR

}


(* regular expressions *)


let one_line_comment =
  '%' [^ '\n' '\r']* ('\n' | "\r\n")

let variable = (* identifiers starting with '=' are parameters *)
  (* prefix and number *)
  ([ '_' '=' ] ['0'-'9']*)
  |
  (* optional prefix and string *)
  ([ '_' '=' ]? [ 'A'-'Z' ] [ 'a'-'z' 'A'-'Z' '0'-'9' '_']*)
  
let symbol =
  ([ 'a'-'z' '0'-'9'] | "__") [ 'a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token =
    parse
      | [' ' '\t' '\r']              { token lexbuf } (* skip blanks *)
      | ['\n']                       { Counter.inc line_counter; token lexbuf } (* skip new lines *)
      | one_line_comment             { Counter.inc line_counter; token lexbuf } (* skip comment *)
      | symbol                       { SYMBOL( Lexing.lexeme lexbuf) }
      | variable                     { VARIABLE( Lexing.lexeme lexbuf) }
      | '('                          { LEFT_PARENTHESIS }
      | ')'                          { RIGHT_PARENTHESIS }
      | '{'                          { LEFT_BRACE }
      | '}'                          { RIGHT_BRACE }
      | '-'                          { NEGATIVE }
      | '+'                          { POSITIVE }
      | ','                          { TERM_SEP }
      | eof                          { EOI (* end of input - for channels, strings, ... *) }
      | _                            { tokenize_error "Invalid Input" (Lexing.lexeme lexbuf) }


{ 

(* footer *)

}
