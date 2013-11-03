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


(** terms, literals, clauses

  All persistent terms are stored in a global database,
  so that each term exists only once in the system (hash-consing).

  Fast access is based on a hash value for every term {!Term.hash_of_term}.
  For variables and constants that's their id,
  for terms it is (for efficiency) stored in the term data structure.

  Benefits: low memory usage by term sharing, fast term comparison by pointer equality.

  Drawbacks: term construction implies a term database access, terms store a hash value.

  Only temporarily needed terms may not be entered into the database,
  if the argument ?insert_db is false in the request functions below.
  By default ?insert_db is true.
*)


(** {6 Types} *)

type var = Var.var
type symbol = Symbol.symbol



(** a functional term, e.g. f(a, b) *)
type func = private {
    symbol: symbol;
    (** the top symbol, e.g. f *)
    
    subterms: term array;
    (** the subterms, e.g. a, b *)

    ground: bool;
    (** is the term ground? *)

    hash: int;
    (** the hash value of this term used in the term database. *)

    in_db: bool;
    (** is this term in the database? *)
}

(** a term can be a variable, a constant or a function. *)
and term = private
  | Var of var
      (** a variable *)

  | Const of symbol
      (** a constant *)

  | Func of func
      (** a function *)


(** a literal is a signed term - the term is interpreted as a predicate. *)
type literal = private {
  sign: bool;
  (** the sign/polarity *)

  atom: term;
  (** the atom *)

  literal_in_db: bool;
  (** is this term in the database? *)
}

(** a clause: a list, not a set as in theory. *)
type clause =
    literal list


(** a specialized [Hashtbl] with terms as keys. *)
module TermTable : Hashtbl.S with type key = term

(** a specialized [Hashtbl] with literals as keys. *)
module LiteralTable : Hashtbl.S with type key = literal

(** a specialized [Hashtbl] with clauses as keys.
    order of literals in a clause does not matter.  *)
module ClauseTable : Hashtbl.S with type key = clause

(** a specialized [Hashtbl] with clauses as keys.
    order of literals in a clause does matter. *)
module ClauseApproxTable : Hashtbl.S with type key = clause

(** a specialized [Hashtbl] with literal types as keys,
  that is not the concrete literal but it's sign and predicate symbol. *)
module LiteralTypeTable : Hashtbl.S with type key = literal




(** {6 Functions} *)

(** {2 Special Terms} *)


(** the {e true} literal (sign is true). *)
val true_literal: literal

(** the {e false} literal (sign is true). *)
val false_literal: literal

(** to be used for default initialization. *)
val null_term: term

(** to be used for default initialization. *)
val null_literal: literal

(** marks an assert gap in a context unifier ({!Context_unifier}). *)
val assert_literal: literal

(** marks an assert gap for a literal of the initial interpretation
    of a context. *)
val init_literal: literal

(** the {e +v} context literal. *)
val plus_v: literal

(** the {e -v} context literal. *)
val minus_v: literal

(** dummy parameter contained in plus_v and minus_v,
    to be used together with [v_term]. *)
val v_par: var

(** dummy term to bind plus_v oder minus_v to in a context unifier,
    see implementation of {!Context_unifier.extend_partial_context_unifier}. *)
val v_term: term


(** {2 Construction} *)

(** requests a {!Term.term}[.Var] term from a database.

    there might be different instances of the same variable {!Var.var} in the system.
    Still, as the first request to {!Term.request_var} creates a database entry
    for this specific instance of the variable,
    and the following requests return this first stored instance,
    all {!Term.term}[.Var]s built on this variable are identical. *)
val request_var: var -> term

(** requests a {!Term.term}[.Const] term from a database. *)
val request_const: symbol -> term

(** requests a {!Term.term}[.Func] term.
    if [insert_db] is true (default: true) the term is inserted into the database,
    if it is not already contained.
    
    stores the original term array, so it might no be modified afterwards.
*)
val request_func: ?insert_db:bool -> symbol * term array -> term


(** requests a {!Term.literal}.
    if [insert_db] is true (default: true) the term is inserted into the database,
    if it is not already contained. *)
val request_literal: ?insert_db:bool -> bool -> term -> literal




(** {2 Term Modification} *)

(** inserts the literal into the database if it is not already contained. *)
val insert_term: term -> term

(** see {!Term.insert_term}. *)
val insert_literal: literal -> literal


(** requests a negation of a literal. *)
val request_negated_literal: ?insert_db:bool -> literal -> literal

(** requests the skolemization of a literal from the database.
    this is the skolemization in the Model Evolution sense,
    i.e each universal variable is replaced by a fresh constant,
    while parameters are normalized
    (each occurrence of the same variable is replaced by the same constant).
*)
val request_skolemized_literal: literal -> literal

(** see {!Term.request_skolemized_literal}. *)
val request_skolemized_clause: clause -> clause

(** requests the universal (or parametric) version of a term from the database,
    i.e. each parameter occuring in the term is replaced
    by a fresh universal variable (or vice versa).
    (each occurrence of the same parameter is replaced by the same variable).

    if universal then parameters are replaced by variables,
    otherwise variables are replaced by parameters.
*)
val request_pure_term: universal:bool -> term -> term

(** see {!Term.request_pure_term}. *)
val request_pure_literal: universal:bool -> literal -> literal

(** see {!Term.request_pure_clause}. *)
val request_pure_clause: universal:bool -> clause -> clause

(** [replace_in_term_term term sub_term replace_term]
    builds a new term from [term] by replacing
    every occurrence of [sub_term] in [term] by [replace_term]. *)
val replace_term_in_term: term -> term ->  term -> term

(** see {!Term.replace_term_in_term}. *)
val replace_term_in_literal: literal -> term -> term -> literal

(** like {!Term.replace_term_in_term}, but for a number of terms simultaneously. *)
val replace_terms_in_term: term -> (term * term) list -> term

(** see {!Term.replace_terms_in_term}. *)
val replace_terms_in_literal: literal -> (term * term) list -> literal

(** [replace_vars_in_term term map]
    like {!Term.replace_terms_in_term},
    but a variable is replaced by a term by the function map.  *)
val replace_vars_in_term: term -> (var -> term -> term) -> term

(** see {!Term.replace_vars_in_term}. *)
val replace_vars_in_literal: literal -> (var -> term -> term) -> literal

(** removes duplicate literals. *)
val remove_duplicates: clause -> clause




(** {2 Term Sorts} *)

(** the sort of the top symbol *)
val get_term_sort: term -> Symbol.sort

(** the sort of the top symbol *)
val get_literal_sort: literal -> Symbol.sort

(** a connection predicate? *)
val is_connection_literal: literal -> bool

(** a finite domain relation predicate? *)
val is_fd_relation: literal -> bool

(** a finite domain marker predicate? *)
val is_fd_size_marker: literal -> bool

(** a finite domain element? *)
val is_fd_element: term -> bool

(** a finite domain constraint literal?
    that is, an equality or domain permutable predicate. *)
val is_fd_constraint: literal -> bool


(** a term built over the input signature? *)
val is_input_term: term -> bool

(** see {!Term.is_input_term} *)
val is_input_literal: literal -> bool

(** a term built over the input signature
    and additional finite domain symbols?
    I.e. {!Symbol.sort} in FD_Relation, FD_Element, FD_Symbol *)
val is_fd_term: term -> bool

(** see {!Term.is_fd_term} *)
val is_fd_literal: literal -> bool

(** a skolem free term? *)
val is_skolem_free_term: term -> bool

(** a skolem free literal? *)
val is_skolem_free_literal: literal -> bool




(** {2 Decomposition} *)


(** the hash value of a term. *)
val hash_of_term: term -> int

(** see {!Term.hash_of_term}. *)
val hash_of_literal: literal -> int

(** see {!Term.hash_of_clause}. *)
val hash_of_clause: clause -> int


(** returns the top symbol of the term.
    @raise Not_found if term is of type [Var]. *)
val top_symbol_term: term -> symbol

(** see {!Term.top_symbol_term}. *)
val top_symbol_literal: literal -> symbol


(** is the term of type [Var]? *)
val is_term_var: term -> bool

(** see {!Term.is_term_var}. *)
val is_literal_var: literal -> bool


(** returns the variables of the term (without duplicates). *)
val vars_of_term: term -> var list

(** see {!Term.vars_of_term}. *)
val vars_of_literal: literal -> var list

(** see {!Term.vars_of_literal}. *)
val vars_of_clause: clause -> var list


(** is the variable contained in the term? *)
val term_contains_var: term -> var -> bool

(** see {!Term.term_contains_var}. *)
val literal_contains_var: literal -> var -> bool

(** [term_contains_term term contained] is the term [contained] contained in the term [term]? *)
val term_contains_term: term -> term -> bool

(** see {!Term.term_contains_var}. *)
val literal_contains_term: literal -> term -> bool


(** is the term ground? *)
val is_term_ground: term -> bool

(** see {!Term.is_literal_var}. *)
val is_literal_ground: literal -> bool

(** {2 Schema Terms} *)

(** a schema term for a symbol is the most general term for this symbol,
    e.g. for a 3-ary symbol f the schema term is f(x, y, z).
*)

(** is this a schema term? *)
val is_schema_term: term -> bool 

(** create the schema term for a symbol. *)
val create_schema_term: symbol -> term

(** create the schema term for a term. *)
val create_schema_term_from_term: term -> term


(** {2 Clause Properties} *)


(** does the clause contain exactly one positive literal? *)
val is_definit: clause -> bool

(** is the clause Horn? *)
val is_Horn: clause -> bool
  
(** is the clause set? *)
val are_Horn: clause list -> bool

(** is the clause free of function symbols? (Bernays-Schoenfinkle class) *)
val is_BS: clause -> bool

(** are the clauses free of function symbols? *)
val are_BS: clause list -> bool

(** is a clause empty, or contains only false? *)
val contains_empty_clause: clause list -> bool

(** does the clause set contain a (dis)equality literal? *)
val contains_equality: clause list -> bool

(** is the clause a tautology?

    tests for:
    - (t = t) \/ C
    - p \/ -p' \/ C where p and p' are variants
*)
val is_tautology: clause -> bool




(** {2 Comparison} *)

(** are two terms equal? *)
val term_equal : term -> term -> bool

(** are two literals equal? *)
val literal_equal : literal -> literal -> bool

(** are two clauses equal? order of literals does not matter. *)
val clause_equal : clause -> clause -> bool

(** are two clauses equal? order of literals does matter. *)
val clause_approx_equal : clause -> clause -> bool

(** a total order on terms. *)
val compare_terms: term -> term -> int

(** see {!Term.compare_terms}. *)
val compare_literals: literal -> literal -> int

(** sorts a clause with {!Term.compare_literals}. *)
val sort_clause: clause -> clause



(** {2 Variants} *)


(** are two terms (universal- and parameter-preserving) variants? *)
val are_terms_variants: term -> term -> bool

(** see {!Term.are_terms_variants}. *)
val are_literals_variants: literal -> literal -> bool

(** are two terms variants modulo skolem constants?
    That is, is there a bijective mapping
    of the skolem constants of one literal to the
    skolem constants of the other literal,
    and the literals are universal- and parameter-preverving variants? *)
val are_literals_skolem_variants: literal -> literal -> bool


(** removes non-equality variants from a literal set *)
val remove_variants: literal list -> literal list

(** removes duplicates from a literal set *)
val remove_duplicates: literal list -> literal list


(** {2 TPTP Conversions} *)


(** replaces all variables in a term by constants,
   so that instead of Darwin's _0 the output is a tptp conform X0. *)
val tptp_replace_var: var -> term

(** performs all transformations necessary to output a term as a tptp term.

    relational function terms of the from r(f, ...)
    are replaced by f(...).
    for r(x, ...), i.e. if there is no fd relation symbol f,
    the function fails with an exception.

    Note: domain elements are named to avoid clashes with the input signature.
    By default, the i.th element is called "ei".
    If this name is already taken then an "e" is prefixed ("eei"),
    and so on until the conflict is resolved.
*)
val to_tptp_term: term -> term


(** {2 String Representation} *)

(** Example: {b +p(a, _0)}.
    pretty is by default true,
    then for example finite domain relations are printed more compact.
*)
val term_to_string: ?pretty:bool -> term -> string

(** Example: {b -p(a, _0)} *)
val literal_to_string: ?pretty:bool -> literal -> string

(** Example: {b \{ +p(f(a), _0), -q(b) \}} *)
val clause_to_string: ?pretty:bool -> clause -> string

(** [tptp_clause_to_tptp_string label clause]
    transforms a clause into a tptp string representation.

    [label] is the label of the clause, e.g. "flattened_0"
*)
val tptp_clause_to_tptp_string: string -> clause -> string
