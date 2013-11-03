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

(** a substitution

  A substitution is an immutable mapping from variables to terms.

  In order to be able to efficiently reuse a literal
  when several fresh variants of it are needed during a unification process,
  each variable and term is paired with an {e offset}.
  Thus creating a fresh variant of a literal
  consists just of pairing the literal with fresh offsets.

  Therefore a {!Subst.subst} stores not a mapping from {!Var.var} to {!Term.term},
  but from {!Subst.var} to {!Subst.term}.
  
  A substitution is represented in the triangular form.
  I.e. if variables occuring in bound terms
  are also bound in the substitution,
  they have to be replaced by their binding.

  E.g. instead of [\[x -> (g(a)); y -> a\]] 
  the substitution may be [\[x -> (g(y)), y -> a\]].

  Path compression is exhaustively done for variable to variable bindings,
  but not for bound variables within a bound term.
  E.g. [\[x -> (g(z)); y -> z; z -> a\]] would be represented as
  [\[x -> (g(z)); y -> a; z -> a\]].

  A substitution is {e parameter preserving (p_preserving)} if
  parameters are only bound injectively to parameters of other terms.

  Thus a {e p_preserving} unifier between two terms
  contains a renaming between the parameters (indicator variables)
  of the two terms.

  If {e p_preserving} is an optional argument of a function below,
  it always defaults to false,
  thus completely ignoring the different variable types.

  if {e recompute} is true as an argument of a function below,
  the unifier is recomputed and is thus known to be computable.
  then consistency checks like the occur check are not performed.
*)



(** {6 Types} *)

(** associates a {!Var.var} with an offset. *)
type var = private {
  sv_var: Var.var;
  sv_offset: int;
}

(** associates a {!Term.term} with an offset. *)
type term = private {
  st_term: Term.term;
  st_offset: int;
}

(** a binding of a {!Subst.var} to a {!Subst.term}. *)
type binding = private {
  sb_var: var;
  sb_term: term;
}
    
(** a substitution is a set of bindings. *)
type subst = binding list


(** a specialized [Hashtbl] with variables as keys. *)
module VarTable : Hashtbl.S with type key = var

(** a substitution as a mapping from variables to bindings.
    In general slower than [subst],
    but for some huge substitutions way faster. *)
type hash_subst = term VarTable.t


(** see {!Subst.set} *)
exception SET_FAIL



(** {6 Functions} *)


(** {2 Offsets} *)
  

(**
    A context unifier is not build at once,
    its literal pairs are unified separately into partial context unifiers
    which are merged afterwards
    ({!Context_unifier_space}, {!Context_unifier_search}).
    Creating a fresh variant of a literal merely consists of
    pairing the literal with an offset.
    Thus in order to be able to correctly merge the partial context unifiers
    they must use different offsets,
    as determined by the following functions:
*)


(** a literal of the input clause. *)
val input_literal_offset: int

(** [context_literal_offset input_clause_literal_index]
  computes the offset of the context literal
  paired with the input literal at position [input_clause_literal_index]
  of the clause.
*)  
val context_literal_offset: int -> int

(** to make a context unifier admissible it might be necessary
  to bind universal variables to new/fresh parameters. *)
val fresh_par_offset: int


(** {2 Creation} *)

(** empty substitution. immutable. *)
val empty: subst

(** create a [Subst.var]. *)
val make_var: Var.var -> int -> var

(** create a [Subst.term]. *)
val make_term: Term.term -> int -> term

(** create a [Subst.binding]. *)
val make_binding: var -> term -> binding


(** {2 Access} *)

(** [get subst var] returns the term bound to [var],
  or None if [var] is unbound.
  Because of the triangular substitution (see module description)
  the returned term may contain other variables bound in the substitution.
  To get the real term its variables binding must be evaluated as well,
  or equivalently {!Subst.apply_to_term} must be applied to the term
  to create the proper term instance. *)
val get: subst -> var -> term option

(** [get' subst var offset] wrapper for {!Subst.get}. *)
val get': subst -> Var.var -> int -> term option

(** [get_bound_vars subst vars] returns for each [var] in [vars]
  - the subst_vars contained in the term bound to [var],
  - or [var] itself if it is unbound. *) 
val get_bound_vars : subst -> var list -> var list  



(** [set subst var term].
  extends [subst] by the binding [var -> term].

  @raise SET_FAIL if
  - the binding would lead to a cycle (occur check)

  - the variable is already set - a binding can not be replaced

  {e p_preserving} is by default false.
  If true, [subst] is expected to be {e p_preserving}
  and is extended with the new binding [var -> term]
  retaining a {e p_preserving} substitution.
  If this is not possible, SET_FAIL is raised.
*)
val set: recompute:bool -> ?p_preserving:bool -> subst -> var -> term -> subst

(** wrapper for {!Subst.set} *)
val set': recompute:bool -> ?p_preserving:bool -> subst -> Var.var -> int -> Term.term -> int -> subst

(** orients bindings by imposing a total order:
    - by offset: <
    - by universal < parametric
    - by var id: <
*)
val orient: binding -> binding

(** dereferences term in the substitution.
    i.e., if the term is a variable and this variable is bound,
    the bound term is returned,
    otherwise the original term.

    performs path compression. *)
val dereference': hash_subst -> term -> term

(** get any binding in the substitution. the substitution may not be empty. *)
val first: subst -> binding

(** find a binding satisfying the predicate *)
val find: (binding -> bool) -> subst -> binding

(** find all bindings satisfying the predicate *)
val find_all: (binding -> bool) -> subst -> subst


(** is this the empty substitution? *)
val is_empty: subst -> bool

(** is this subset p-preserving on the given offset?
    I.e. after a unification it can be checked if a term paired with this offset
    is instantiated only in a p-preserving way. *)
val is_p_renaming: subst -> int -> bool

(** the number of bindings in the substitution. *)
val length: subst -> int


(** adds the binding to the substitution without doing any of the consistency checks
    that {!Subst.set} does perform. *)
val append: binding -> subst -> subst

(** like [List.map], but the result must be a substitution. *)
val map: (binding -> binding) -> subst -> subst

(** like [List.map], but the result may be any list. *)
val map': (binding -> 'a) -> subst -> 'a list

(** like [List.partition] *)
val partition: (binding -> bool) -> subst -> subst * subst

(** like [List.for_all] *)
val for_all: (binding -> bool) -> subst -> bool

(** like [List.exists] *)
val exists: (binding -> bool) -> subst -> bool

(** like [List.fold] *)
val fold: ('a -> binding -> 'a) -> 'a -> subst -> 'a

(** like [List.iter] *)
val iter: (binding -> unit) -> subst -> unit



(** {2 Access - Modules} *)

(** In functions which heavily rely on substitution access
    it pays of to hard code the {e p_preserving} options
    in terms of performance.
    For these means the following modules specialize on the [set] function:
*)

(** interface for specialized set modules *)
module type T_Preserving = 
sig
  val set: subst -> var -> term -> subst
  val set': subst -> Var.var -> int -> Term.term -> int -> subst
end

(** recompute = false; p_preserving = true *)
module PPreserving : T_Preserving

(** recompute = false; p_preserving = false *)
module Preserving : T_Preserving

(** recompute = true; p_preserving = true *)
module RPPreserving : T_Preserving

(** recompute = true; p_preserving = false *)
module RPreserving : T_Preserving







(** {2 Application} *)



(** returns the normalized version of the term.
    I.e. the variables' ids in the new term are enumerated from 0 on. *)
val normalize_term: Term.term -> Term.term

(** see {!Subst.normalize_term}. *)
val normalize_literal: Term.literal -> Term.literal

(** see {!Subst.normalize_term}. *)
val normalize_clause: Term.clause -> Term.clause

(** [apply_to_term subst term offset]
    applies [subst] to [term] with [offset] and returns a new term.
  
    for [insert_db] see {!Term}

    if [normalize] is true the term is normalized (default:true) .
*)
val apply_to_term: ?insert_db:bool -> ?normalize:bool -> subst -> Term.term -> int -> Term.term

(** see {!Subst.apply_to_term}. *)
val apply_to_literal: ?insert_db:bool -> ?normalize:bool -> subst -> Term.literal -> int -> Term.literal

(** see {!Subst.apply_to_term}. *)
val apply_to_clause: subst -> Term.clause -> int -> Term.clause

(** like {!Subst.apply_to_term}.
    the normalization is done consistently over all literals,
    i.e. a variable occuring in several term instances
    is mapped to the same literal in the normalized version.
    E.g. [apply_to_literals \[ 0:x -> 2:z; 1:y -> 2:z \] \[ (p(x), 0); (q(y), 1) \]]
    returns [\[ p(x), q(x) \]], where [x] has the id 0.
*)
val apply_to_literals: subst -> (Term.literal * int) list -> Term.literal list


(** like {!Subst.apply_to_literals}, but for lists of literals. *)
val apply_to_literals_groups: subst -> (Term.literal * int) list list -> Term.literal list list

(** like {!Subst.apply_to_literals}, but using a [hash_subst]. *)
val apply_to_literal_groups': hash_subst -> (Term.literal * int) list list -> Term.literal list list


(** [replace_offset subst old_offset new_offset]
  replaces all occurences of [old_offset] in [subst]
  by [new_offset]. *)
val replace_offset: subst -> int -> int -> subst

(** [replace_offset subst old_term new_term]
  replaces in all terms bound in [subst]
  all occurences of [old_term] by [new_term].

  No offsets can be specified and there may only be {b one}
  offset used in the whole substitution. *)
val replace_in_bound_terms: subst -> Term.term -> Term.term -> subst


(** [reverse_var_to_par subst vars_to_reverse pars_to_keep]
  reverses one by one the bindings of the variables in [vars_to_reverse], if
  - the variable is bound to a parameter
  - no variable in [vars_to_keep] is bound to the same parameter as the variable -
    this would also reverse the parameters binding and let it be bound to a universal variable,
    thus loosing p_preserving bindings.
*)
val reverse_bindings: subst -> var list -> var list -> subst

(** removes all renamings of context variables
    that are not bound by/in a term bound by another variable.
    e.g.
    u -> a; v -> x; w -> y; z -> f(w)
    becomes
    u -> a;         w -> y; z -> f(w)
*)
val remove_context_var_renamings: subst -> subst



(** {2 Comparison} *)

(** are the two variables identical? *)
val var_equal : var -> var -> bool

(** are the two terms identical? *)
val term_equal: term -> term -> bool

(** are the two bindings identical? *)
val binding_equal: binding -> binding -> bool

(** are the two substitutions identical? *)
val subst_equal: subst -> subst -> bool

(** usual comparison function *)
val subst_compare: subst -> subst -> int


(** {2 String Representation} *)

val var_to_string: var -> string

val term_to_string: term -> string

val binding_to_string: binding -> string

(** returns the triangular form,
  i.e. bound variables are not replaced by their bound term. *)
val subst_to_string: subst -> string
