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


(** reads tme files

  See {{:http://www.uni-koblenz.de/ag-ki/Systems/Protein/tme-syntax.txt}Backus
  Naur tme Syntax} for the complete tme specification.

  all functions @raise Const.PARSE_ERROR on incorrect input.

  Only the subset necessary to read formula is supported in darwin,
  fancy things like {e costs}, {e flags}, ... are not.

  E.g. MSC006-1 from TPTPv2.6:
{[
  q(X, Z) :- 
    q(X, Y), 
    q(Y, Z).
]}

{[
  p(X, Y); 
  q(X, Y).
]}

{[
  false :- p(a, b).
]}

  The supported tme subset in EBNF:

{[
clause    ::= head [ ( ':-' | '<-' ) body ]
head      ::= literal { ( ',' | ';' ) literal }
body      ::= literal { ',' literal }
literal   ::= [ ('-' | '~') ] atom
            | 'true'
            | 'false'
atom      ::= symbol
            | symbol '(' term { ',' term } ')'
            | '(' term '=' term ')'
term      ::= variable
            | symbol [ '(' term { ',' term } ')' ]?
variable  ::= ( 'A'-'Z' | '_' ) { 'a'-'z' | 'A'-'Z' | '0'-'9' | '_' }
symbol    ::= ( 'a'-'z' | '0'-'9' ) { 'a'-'z' | 'A'-'Z' | '0'-'9' | '_' }
]}

  [%] is used for line comments, [/*] ... [*/] to comment regions.

  Parameteric variables can not be represented in a tme file. *) 
type clause = Term.clause

(** [to_clauses_from_file file_name] reads the tme file [file_name]
  into a {!Term.clause} list.

  On a parsing error a [Failure] exception is raised. *)
val to_clauses_from_string : string -> clause list

(** [to_clauses_from_file file_name] reads the tme file [file_name]
  into a {!Term.clause} list.

  On a parsing error a [Failure] exception is raised. *)
val to_clauses_from_file : string -> clause list

(** converts a string to a {!Term.clause}. *)
val to_clause : string -> clause
