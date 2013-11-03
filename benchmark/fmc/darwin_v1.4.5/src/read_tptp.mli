(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2006  The University of Iowa

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


(** reads tptp files 

  See {{:http://www.cs.miami.edu/~tptp/TPTP/SyntaxBNF.html}BNF}
  for the complete tptp specification,
  implemented is version 3.1.1.27.

  Only the CNF subset is supported.

  Parameteric variables can not be represented in a tptp file. *) 
type clause = Term.clause


(** [to_clauses_from_file file_name] reads the tptp file [file_name]
  into a {!Term.clause} list.

  @raise Const.PARSE_ERROR on incorrect input.

  @raise Const.FOF_INPUT on fof input.
*)
val to_clauses_from_string : string -> clause list

(** [to_clauses_from_file file_name] reads the tptp file [file_name]
  into a {!Term.clause} list.

  @raise Const.PARSE_ERROR on incorrect input.

  @raise Const.FOF_INPUT on fof input.
*)
val to_clauses_from_file : string -> clause list
