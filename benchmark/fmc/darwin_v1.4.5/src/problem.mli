(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2006
              The University of Iowa

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

(** a problem clause set *)


type symbol = Symbol.symbol
type literal = Term.literal
type clause = Term.clause

(** mapping from arity to symbols.
    when returned by a method of class {!Problem.problem}
    sorted increasingly by arity.
*)
type arities =
    (int * (symbol list)) list

(** a problem clause set *)
class type problem =
object

  (** {2 Clauses} *)

  (** adds clauses (preserving their order) to front of current clauses *)
  method addClauses: clause list -> unit

  (** see {!Problem.problem.addClauses}. *)
  method addClause: clause -> unit

  (** adds literals (preserving their order) to the initial interpretation. *)
  method addToInitialInterpretation: literal list -> unit

  (** returns the clause set. *)
  method getClauses: clause list

  (** returns the initial interpretation.
      this should be used to initialize the context. *)
  method getInitialInterpretation: literal list


  (** {2 Properties} *)


  (** returns the predicate symbols contained in the clause set
      (except for {!Symbol.equality}). *)
  method getPredicateSymbols: symbol list

  (** returns the function symbols contained in the clause set *)
  method getFunctionSymbols: symbol list

  (** returns the constant symbols contained in the clause set *)
  method getConstantSymbols: symbol list

  (** returns the predicate symbols (except for {!Symbol.equality}) grouped by arity.
      ordered by increasing arity. *)
  method getPredicateArities: arities

  (** returns the function symbols grouped by arity.
      ordered by increasing arity. *)
  method getFunctionArities: arities



  (** like getPredicateSymbols, but includes also symbols from [simplified] **)
  method getAllPredicateSymbols: symbol list

  (** like getFunctionSymbols, but includes also symbols from [simplified] **)
  method getAllFunctionSymbols: symbol list

  (** like getConstantSymbols, but includes also symbols from [simplified] **)
  method getAllConstantSymbols: symbol list

  (** like getPredicateArities, but includes also symbols from [simplified] **)
  method getAllPredicateArities: arities

  (** like getFunctionArities, but includes also symbols from [simplified] **)
  method getAllFunctionArities: arities



  (** returns the length of the longest clause. *)
  method getMaxClauseLength: int

  (** does the clause set contain {!Symbol.equality}? *)
  method containsEquality: bool

  (** is the clause set Horn? *)
  method isHorn: bool

  (** is the clause set free of function symbols (Bernays-Schoenfinkle) ? *)
  method isBS: bool
end

(** [create equality bool clauses simplified initial_interpretation]
    creates an instance of class type problem.

    [equality] is false and [horn] is true by default,
    but if set otherwise this will override [clauses].

    [initial_interpretation] is a literal set which is assumed to be true
    in any context (see Preprocessing_pure).

    [simplified] contains clauses and literals remove from [clauses]
    in preprocessing.

    This is for the finite model mode,
    as the transformation might have removed the equality axioms,
    and as non-Horn axioms are added later on.
*)
val create: ?equality:bool -> ?horn:bool -> clause list -> clause list list -> literal list -> problem

(** adds symbol to arities,
    that is to list of symbols with the same arity
    while keeping arities in increasing order. *)
val update_arities: arities -> symbol -> arities

(** returns all symbols of the given arity *)
val get_arity: arities -> int -> symbol list
