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


(** constant and function symbols

  A symbol consists of
  - a sort
  - a unique name (eg {e p}, {e f} or {e a})
  - an arity (0 for a constant)
  - a unique id
    
  All symbols are irrevocably registered in a global symbol table.
*)


(** {6 Types} *)

(** Thrown if a symbol is created several times,
    with different arities/sorts.
    May be thrown by all symbol creation functions. *)
exception OVERLOADING

(** each symbol has a sort.
    the basic sorts are predicates and functions from the input signature.
    In addition there are various sorts generated in the system,
    in skolemization, preprocessing, ...
*)
type sort =
  | Predicate
      (** a predicate symbol of the input signature *)

  | Function
      (** a function symbol of the input signature *)

  | Skolem
      (** a skolem constant *)

  | Connection
      (** a predicate that connects clauses split in preprocessing *)

  | FD_Relation
      (** finite model finding: relational representation of functions (predicate).
      for each arity exists a unique relation symbol. *)

  | FD_Size_Marker
      (** finite model finding: a marker for the domain size (predicate) *)

  | FD_Element
      (** finite model finding: domain element (constant) *)

  | FD_Symbol
      (** finite model finding: symbol of original signature transformed into a constant *)

  | FD_Permutable
      (** finite model finding: permutable domain element axiom *)

  | FD_Diff
      (** -diff instead of = *)

(** a symbol *)
type symbol


(** a specialized [Hashtbl] with symbols as keys. *)
module SymbolTable : Hashtbl.S with type key = symbol



(** {6 Functions} *)



(** {2 Special Symbols} *)

(** these special symbols are automatically added to the symbol table. *)

(** the equality symbol *)
val equality: symbol

(** the negated equality symbol *)
val diff: symbol

(** used to denote the pseudo-predicate standing for the closed clause,
    when computing a lemma. *)
val lemma_root: symbol



(** {2 Creation} *)


(** [Symbol.create_predicate name arity] registers a predicate symbol and returns it. *)
val create_predicate: string -> int -> symbol

(** [Symbol.create_function name arity] registers a function symbol and returns it. *)
val create_function: string -> int -> symbol

(** creates a unique fresh skolem symbol.
    [arity] is 0 by default, so a skolem constant will be created. *)
val create_skolem: ?arity:int -> ?name:string option -> unit -> symbol

(** creates a unique fresh connection symbol of the given arity. *)
val create_connection: int -> symbol

(** returns the relation symbol of the given arity.

    this symbol has the given arity,
    that is it represents function symbols of arity - 2.
*)
val get_fd_relation: int -> symbol

(** returns the maker symbol of the given domain size. *)
val get_fd_size_marker: int -> symbol

(** returns the i.th domain element.
    to_string ~pretty returns i. *)
val get_fd_element: int -> symbol

(** returns the constant symbol which is the transformation
    of the given constant/function symbol. *)
val create_fd_symbol: symbol -> symbol

(** returns the unary predicate symbol
    used to axiomatize if a domain element is permutable or not.
*)
val create_fd_permutable: int -> symbol

(** reverse function to {!Symbol.create_fd_symbol}.
    fails with an exception if {!Symbol.create_fd_symbol}
    has not been called before. *)
val get_symbol_from_fd_symbol: symbol -> symbol


(** {2 Decomposition} *)

(** the symbol's unique name. *)
val name: symbol -> string

(** the symbol arity. *)
val arity: symbol -> int

(** the symbol' uniqe id. Useful for hashing on symbols or terms. *)
val id: symbol -> int

(** the symbol's sort *)
val sort: symbol -> sort


(** {2 Comparison} *)

(** the same symbol? *)
val equal: symbol -> symbol -> bool

(** total order by id. *)
val compare: symbol -> symbol -> int

(** total order by name. *)
val compare_name: symbol -> symbol -> int

(** a predicate symbol (sort = Predicate)? *)
val is_predicate: symbol -> bool

(** a function symbol (sort = Function)? *)
val is_function: symbol -> bool

(** a symbol from the input signature?
    Same as is_predicate || is_function. *)
val is_input: symbol -> bool

(** a skolem symbol? *)
val is_skolem: symbol -> bool

(** a connection symbol? *)
val is_connection: symbol -> bool

(** a finite domain relation symbol? *)
val is_fd_relation: symbol -> bool

(** a finite domain marker symbol? *)
val is_fd_size_marker: symbol -> bool

(** a finite domain element symbol? *)
val is_fd_element: symbol -> bool

(** a finite domain function symbol? *)
val is_fd_symbol: symbol -> bool

(** a finite domain permutation symbol? *)
val is_fd_permutable: symbol -> bool


(** {2 String Representation} *)

(** print a symbol.
    pretty (default: true) printing prints
    e.g. domain elements as '1 instead of __fd1 *)
val to_string: ?pretty:bool -> symbol -> string

(** string representation of a sort. *)
val sort_to_string: sort -> string

val get_by_name: string -> symbol
