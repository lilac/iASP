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



(** iterative deepening bound *)



(** {6 Types} *)

type config = Config.config
type literal = Term.literal
type choice_point = State.choice_point
type subst = Subst.subst


(** the complexity (of a literal),
    i.e. the value to be checked against the deepening bound.
    Currently the term depth or weight ({!Flags.opt_iterative_deepening}).
*)
type complexity = int


(** Here all candidates are checked for {e exceeding} the iterative deepening bound.
    This includes determining if the current derivation branch is {e incomplete},
    i.e. if candidates have been dropped while constructing the branch,
    and (based on {!Flags.opt_restart}) backtracking to a choice point,
    where the branch is not incomplete.
    
    The actual bound criterion is handled internally according
    to {!Flags.opt_iterative_deepening}.

    In the finite domain mode ({!Config.finite_domain})
    the bound does not correspond to {!Flags.opt_iterative_deepening},
    but to the current domain size.
    As the input is BS then,
    any the complexity of any literal will be within the bound.
*)
class type bound =
object
  (** {2 Member Access} *)


  (** the current deepening bound. *)
  method current_bound: complexity
   
  (** the oldest choice point of an ignored literal in the current derivation branch,
      if any have been ignored yet.
      this implies that the current branch can not be extended towards a model,
      but backtracking before this choice point might be. *)
  method dropped_choice_point : choice_point option



  (** {2 Complexity} *)
    
  (** the complexity value of a literal. *)
  method get_complexity : literal -> complexity
    
  (** [get_complexity_subst literal subst offset]
      returns the complexity of the literal assembled by applying
      [subst] with [offset] to [literal]. *)
  method get_complexity_subst : literal -> subst -> int -> complexity
    
  (** usual comparison.
      Amounts to a compare over ints for term depth and term weight. *)
  method compare_complexity : complexity -> complexity -> int
    

    
  (** {2 Check} *)
    
  (** returns true, if the candidate exceeds the current deepening bound. *)
  method exceeds : complexity -> bool
    
  (** returns true, if registering this candidate would have an impact
      wrt. to backtracking over an incomplete branch.
      that is,
      if the dropped complexity is higher than any complexity dropped up to now,
      or if the drop occurs in an older choice point than any other drop yet ({!Bound.bound.dropped_choice_point}).
      
      This is never the case if the candidate does not exceed the deepening bound.
      Otherwise, the details depend on the restart strategy ({!Flags.opt_restart}),
      e.g. for eager restarting only the lowest dropped complexity is of relevance.
  *)
  method exceeds_current : complexity -> choice_point -> bool
    
  (** like {!Bound.bound.exceeds},
      and additionally registers the candidate as dropped. *)
  method register : complexity -> choice_point -> bool
  
  (** like {!Bound.bound.register},
      but for the literal assembled by applying [subst] with [offset] to [literal]. *)
  method register_subst : literal -> subst -> int -> choice_point -> bool

  (** restarting will increase the depth bound by 1. *)
  method has_min_exceeding: bool

    
  (** {2 Backtracking} *)


  (** retracts information invalidated by backtracking ({!State}). *)
  method backtrack : unit

  (** cleans all stored data and increases the depth bound (if [keep_bound] is false, which is the default).
      The new depth bound is the lowest complexity of the dropped candidates. *)
  method restart : keep_bound:bool -> unit



  (** {2 Incomplete Branches} *)
  
  (** an incomplete branch is part of the derivation and has been skipped.
      This implies that the current derivation can not be extended towards a refutation. *)
  method is_derivation_incomplete : bool

  (** marks the derivation as incomplete, i.e. an incomplete branch is (going to be) skipped. *)
  method incomplete : unit




  (** {2 Representation} *)
  
  
  (** the deepening bound as a string. *)
  method complexity_to_string: complexity -> string
  
  (** string representation of the current bound, restarts, and skipped incomplete branches
      (ala {!Print.print_label}). *)
  method print_statistic : unit
end


(** {2 Creation} *)

(** creates a [bound] object with the initial bound given in {!Config.deepening_bound}.
    if [inc] is true, {!Bound.bound.restart} always increases the bound by 1,
    otherwise as described in {!Bound.bound.restart} *)
val create: inc:bool -> config -> bound

(** creates a [bound] object specialized for the BS class (function free terms)
    using term depth as the complexity.

    that is, all terms are known to be of term depth 2,
    so in essence no bound checks need to be performed at all.

    the first argument is a hack:
    in finite domain mode it representes the initial domain size,
    and current_bound returns the current domain size (after restarts).
*)
val create_BS: int -> config -> bound
