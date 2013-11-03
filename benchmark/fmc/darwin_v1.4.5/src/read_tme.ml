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


type clause = Term.clause


let to_clauses_from_string (source: string) : clause list =
  let channel =
    Lexing.from_string source
  in
    try
      Parser_tme.parse_file Lexer_tme.token channel
    with
      | Parsing.Parse_error ->
	  Lexer_tme.parse_error ()


let to_clauses_from_file (file_name: string) : clause list =
  let channel =
    Lexing.from_channel (open_in file_name)
  in
    try
      Parser_tme.parse_file Lexer_tme.token channel
    with
      | Parsing.Parse_error ->
	  Lexer_tme.parse_error ()


let to_clause (string: string) : clause =
  Parser_tme.parse_clause Lexer_tme.token (Lexing.from_string string)
