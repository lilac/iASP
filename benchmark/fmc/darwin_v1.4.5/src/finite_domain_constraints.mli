type literal = Term.literal
type clause = Term.clause
type subst = Subst.subst
type finite_domain = Finite_domain.finite_domain

type constraints
type search
type solution


val create: finite_domain -> clause -> constraints


(** for constraints *)


(** statically unsatisfiable in the current domain size.
    checked on creation [create], so zero time operation. *)
val is_unsatisfiable: constraints -> bool

(** bindings of constraint variables to domain elements
   which are required to find a solution. *)   
val base_subst: constraints -> subst



(** for specific subsitution *)

(** assumes that base_subst is part of subst *)
val setup: constraints -> subst -> search


(** quick check if any two constraint variables which have to be disjoint
   are instantiated to the same value by subst. *)
val is_unsatisfiable_filter: constraints -> subst -> bool





(** will the instantiation of the constraint variables
   extend a p-preserving unifier to a p-preserving one?

   returns true if [is_unsatisfiable] is true,
   as there will be no solution in the first place.
   so check this first.
*)
val is_p_preserving: constraints -> subst -> bool


(* if there is a solution which extends subst p-preservingly,
   return the extended subst + the solution.

   so assumes that subst has an empty remainder on the non-constraint
   part of the clause.
*)
(*val is_closing: search -> (subst * subst option) option*)

(* returns the first solution, if one exists *)
val exists_solution: search -> (subst * solution option) option

(* for a substitution, returns all substitutions,
   as instantiation of the original substitution
   +
   a substitution restricted to a solution of the constraints
   (except for the variables which were already bound in the original substitution) *)
val get_solutions: search -> (subst * solution option) list





(** conversion of constraint solution into substitution form *)
val solution_to_subst: solution -> subst

(** comparision of constraint solutions *)
val solution_compare: solution -> solution -> int



(** the original clause *)
val get_clause: constraints -> clause
(** the unconstrained part of the clause *)
val get_unconstrained: constraints -> clause
(** the constrained part of the clause *)
val get_constrained: constraints -> clause

