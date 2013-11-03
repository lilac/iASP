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

(** infers sorts based on a clause set

    Implements sort inference, see the paper
    New Techniques that Improve MACE-style Finite Model Finding
    by
    Koen Classen and Niklas Soerensson.
    
    For an unsorted first-order clause set,
    the sorts of each function and predicate symbol (arguments and results)
    are first assumed to be different.
    Then sorts are collapsed,
    if their occurrences in the clauses require two sorts to be identical.
    For example,
    - g(a) and g(b) requires a and b to have the same sort,
    - f(x, x) and f(a, b) requires a and b to have the same sort,
    - (a = b) requires a and b to have the same sort.
    
    Sorts can be used for static symmetry reduction ({!Finite_domain}).
*)

(** {6 Types} *)


type symbol = Symbol.symbol
type var = Var.var
type clause = Term.clause

(** the sort of a symbol signature,
    e.g. 'A for a constant,
    or 'A and 'B for a unary function with signature 'A -> 'B. *)
type sort

(** the sorts of a clause set. *)
type sorts


(** a specialized [Hashtbl] with sorts as keys. *)
module SortTable : Hashtbl.S with type key = sort


(** {6 Functions} *)

val sort_equal: sort -> sort -> bool

(** infers the sorts for the clause set. *)
val infer: print:bool -> clause list -> sorts

(** returns the inferred sorts *)
val get_sorts: sorts -> sort list

(** returns the sort of the i.th argument of symbol,
    starting from 0. *)
val get_argument_sort: sorts -> symbol -> int -> sort

(** returns None, if the sort can not be determined.
    This means the variable occurs in an equality
    that is completely disconnected from any non-equality terms,
    i.e. conceptually of its own sort. *)
val get_var_sort: sorts -> clause -> var -> sort option

(** return a special sort used for variables which occur only
    in equality literals in a clause,
    and for which no sort can be deduced. *)
val var_sort: sort

(** [add_constant sorts constant existing]
    registers the new [constant] symbol.
    Its sort is of the result sort of the symbol [existing],
    which must already be registerd.
*)
val add_constant: sorts -> symbol -> symbol -> unit



(** returns the constants partitioned by sorts. *)
val constants_partition: sorts -> symbol list array

(** returns the biggest partition in {!Sort_inference.constants_partition}. *)
val max_constant_partition_size: sorts -> int



(** returns the constants of the given sort *)
val get_constants_for_sort: sorts -> sort -> symbol list

(** returns an unary function symbol of the given result sort,
    if there is such a function symbol *)
val get_unary_function_for_sort: sorts -> sort -> symbol option


(** get the symbol used to axiomatize
    that a domain element of this sort is (or is not)
    permutable in an isomorphic model
    (see {!Const.fd_isomorphism_abstraction}). *)
val get_permutable: sort -> symbol

(** are there permutable elements for this sort?
    in principle, that is, not necessarily within the current domain size. *)
val exists_permutable: sorts -> sort -> bool

(** returns the minimum permutable domain element.
    this might be outside of the current domain size, though. *)
val get_min_permutable: sorts -> sort -> int

(** is the domain element corresponding to this symbol
    (as retrived by [get_permutable sort?])
    permutable?
*)
val is_permutable: sorts -> sort -> symbol -> bool

(** is the i.th domain element of the given sort permutable? *)
val is_permutable_: sorts -> sort -> int -> bool


(** returns the size of the max clique of disequalities *)
val get_max_clique: sorts -> int

(** returns the size of the max clique for the given sort *)
val get_max_clique_for_sort: sorts -> sort -> int



(** for a predicate, return the predicate symbol,
    for a 'relational function', return the 'relationalized' function symbol.

    may not be called on a relational predicate generalizing over several function,
    e.g. r_2(y, x0, x1, z)
    instead of r_2(f, x0, x1, z)
*)
val get_function_symbol_of_func: Term.func ->  symbol


(** prints the sorts. *)
val print: sorts -> unit

val sort_to_string: sort -> string

(* bool: una, all domain constants are pairwise disequal *)
val compute_clique: sorts -> bool -> clause list -> unit
