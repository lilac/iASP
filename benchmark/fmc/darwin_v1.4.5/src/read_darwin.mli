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


(** reads files in darwin's format

  This input format was mainly implemented for quick testing and debugging.
  Every formula is seen as a clause.
  White space between tokens is ignored.

  All functions @raise Const.PARSE_ERROR on incorrect input.

  - Variable: starts with an upper case letter, e.g. {e X}
  - Universal Variable: may be prefixed with a {e _}, e.g. {e _X}
    if prefixed a number is also a valid name, e.g. {e _0}
  - Parametric Variable: must be prefixed with a {e =}, e.g. {e =U}
    if prefixed a number is also a valid name, e.g. {e =0}
  - Symbols: starts with a lower case letter or a number,
    e.g. {e a}, {e barrel}, {e 0}
  - Term: uses {e (}, {e )} to enclose its arguments,
    e.g. {e p(X, a)}
  - Literal: like a term,
    prefix {e -} for a negative literal,
    prefix {e +} or nothing for a positive literal,
    e.g. {e -p(X, a) }
  - Clause: e.g. {e \{ p(X), -p(a) \} }
  - Clauses: e.g. {e \{ p(X) \} \{ -p(a) \} }
*)


type var = Var.var
type term = Term.term
type literal = Term.literal
type clause = Term.clause




(** converts a string to a {!Var.var}. *)
val to_var : string -> var

(** converts a string to a {!Term.term}. *)
val to_term : string -> term

(** converts a string to a {!Term.literal}. *)
val to_literal : string -> literal

(** converts a string to a {!Term.clause}. *)
val to_clause : string -> clause

(** [to_clauses string] converts [string] into a {!Term.clause} list *)
val to_clauses : string -> clause list

(** [to_clauses_from_file file_name] reads the file [file_name]
  and converts it into a {!Term.clause} list *)
val to_clauses_from_string : string -> clause list

(** [to_clauses_from_file file_name] reads the file [file_name]
  and converts it into a {!Term.clause} list *)
val to_clauses_from_file : string -> clause list
