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




(*** types ***)


type config = Config.config
type choice_point = State.choice_point
type term = Term.term
type literal = Term.literal
type subst = Subst.subst

type complexity = int


class type bound =
object
  method current_bound: complexity
  method dropped_choice_point : choice_point option

  method get_complexity : literal -> complexity
  method get_complexity_subst : literal -> subst -> int -> complexity
  method compare_complexity : complexity -> complexity -> int

  method exceeds : complexity -> bool
  method exceeds_current : complexity -> choice_point -> bool
  method register : complexity -> choice_point -> bool
  method register_subst : literal -> subst -> int -> choice_point -> bool
  method has_min_exceeding : bool

  method backtrack : unit
  method restart : keep_bound:bool -> unit
  method is_derivation_incomplete : bool
  method incomplete : unit

  method complexity_to_string: complexity -> string
  method print_statistic : unit
end

class bound_generic ~(inc:bool) (config: config) =
object (self)

  initializer
    ();

  (* the current bound *)
  val mutable bound: complexity = Config.deepening_bound config;

  (* how many incomplete exhausted branches have been found and backtracked from? *)
  val mutable incomplete_branches: int = 0;

  (* how many restarts with an increased depth bound have been done? *)
  val mutable restarts: int = 0;

  (* is the current derivation incomplete,
     i.e. an incomplete exhausted branch has been found, but was ignored? *)
  val mutable derivation_incomplete: bool = false;

  (* lowest complexity of among the dropped candidates. *)
  val mutable dropped_complexity: complexity option = None;

  (* oldest choice point among the dropped candidates. *)
  val mutable dropped_choice_point: choice_point option = None;


  (*** weighting functions ***)

  method complexity_of_literal =
    match Config.iterative_deepening config with
      | Flags.IT_TermDepth -> Term_attributes.depth_of_literal
      | Flags.IT_TermWeight -> Term_attributes.weight_of_literal
  
  method complexity_of_literal_subst =
    match Config.iterative_deepening config with
      | Flags.IT_TermDepth -> Term_attributes.depth_of_literal_subst
      | Flags.IT_TermWeight -> Term_attributes.weight_of_literal_subst

  method bounded_complexity_of_literal =
    match Config.iterative_deepening config with
      | Flags.IT_TermDepth -> Term_attributes.bounded_depth_of_literal
      | Flags.IT_TermWeight -> Term_attributes.bounded_weight_of_literal
  
  method bounded_complexity_of_literal_subst =
    match Config.iterative_deepening config with
      | Flags.IT_TermDepth -> Term_attributes.bounded_depth_of_literal_subst
      | Flags.IT_TermWeight -> Term_attributes.bounded_weight_of_literal_subst


  (*** access ***)

  method current_bound : complexity = bound
    
  method dropped_complexity : complexity option = dropped_complexity
  
  method dropped_choice_point : choice_point option = dropped_choice_point


  (*** complexity ***)
    
  method get_complexity (literal: literal) : complexity =
    match dropped_complexity with
      | None ->
	  (* get its real depths,
	     we want to increase to the highest possible depths bound,
	     not just the current one + 1. *)
	  self#complexity_of_literal literal
	    
      | Some bound ->
	  (* we are not interested in any higher bound then the current
	     dropped depth bound, so cut the search there *)
	  self#bounded_complexity_of_literal literal bound


  method get_complexity_subst (literal: literal) (subst: subst) (offset: int) : complexity =
    match dropped_complexity with
      | None ->
	  self#complexity_of_literal_subst literal subst offset

      | Some bound ->
	  self#bounded_complexity_of_literal_subst literal subst offset bound

      
  method compare_complexity (complexity1: complexity) (complexity2: complexity) : int =
    Tools.compare_int complexity1 complexity2



  (*** check ***)



  (* does the literal exceed the deepening bound? *)
  method exceeds (complexity: complexity) : bool =
    bound < complexity


  (* does this exceeding candidate have a lower complexity
     than all up to now dropped candidates? *)
  method exceeds_current_bound (complexity: complexity) : bool =
    match dropped_complexity with
      | None ->
	  (* first exceeding candidate *)
	  true
	    
      | Some dropped_complexity ->
	  (* don't want to increment to the next complexity level *)
	  not inc
	  &&
	  (* lowest candidate? *)
	  self#compare_complexity dropped_complexity complexity = 1

  (* for backtracking over an incomplete branch:
     is the choice point of this candidate of any relevance? *)
  method exceeds_current_choice_point (choice_point: choice_point) : bool =
    match dropped_choice_point with
      | None ->
	  true	  

      | Some dropped_choice_point ->
	  State.compare_age choice_point dropped_choice_point == -1


  method exceeds_current (complexity: complexity) (choice_point: choice_point) : bool =
    self#exceeds complexity
    &&
    (
      (* exceeding complexity *)
      self#exceeds_current_bound complexity
      ||
      (* older choice point *)
      self#exceeds_current_choice_point choice_point
    )


  (* the literal is dropped if it exceeds the bound
     and the complexity and choice point are registered for restarting. *)
  (*  method private virtual do_register: complexity -> choice_point -> bool*)
  method register (complexity: complexity) (choice_point: choice_point) : bool =
    if self#exceeds complexity then begin
      (* update the minimal dropped complexity. *)
      if self#exceeds_current_bound complexity then begin
	dropped_complexity <- Some complexity
      end;
	
      (* update the dropped choice point. *)
      if self#exceeds_current_choice_point choice_point then begin
	dropped_choice_point <- Some choice_point;
      end;
    
      true
    end
    
    else begin
      false
    end
    
  method register_subst (literal: literal) (subst: subst) (offset: int) (choice_point: choice_point) : bool =
    let complexity =
      self#get_complexity_subst literal subst offset
    in
      self#register complexity choice_point


  (* restarting will increase the bound by 1 *)
  method has_min_exceeding : bool =
    (* either always restart with next complexity level *)
    (inc && dropped_choice_point != None)
    ||
    (* or check that we already need to restart with the next complexity level *)
    match dropped_complexity with
      | None -> false	    
      | Some dropped_bound -> bound == dropped_bound - 1


  (*** backtracking ***)

  method backtrack : unit =
    match dropped_choice_point with
      | Some choice_point when
	    State.is_choice_point_invalid choice_point ->
	  dropped_choice_point <- None
	  
      | _ ->
	  ()


  method restart ~(keep_bound: bool) : unit =
    if not keep_bound then begin
      (* for finite domain always increase bound only by 1 *)
      if inc || Config.finite_domain config then begin
        bound <- bound + 1;
      end

      else begin
        match dropped_complexity with
	  | None ->
	      failwith "Bound.bound_generic.restart"
	      
  	  | Some dropped_complexity ->
	      bound <- dropped_complexity
      end
    end;

    restarts <- restarts + 1;
    derivation_incomplete <- false;
    dropped_complexity <- None;
    dropped_choice_point <- None


  (*** incompleteness ***)

  method is_derivation_incomplete : bool =
    derivation_incomplete


  method incomplete : unit =
    derivation_incomplete <- true;
    incomplete_branches <- incomplete_branches + 1


  (*** representation ***)

  method complexity_to_string (complexity: complexity) : string =
    string_of_int complexity

  method print_statistic : unit =
    Print.print_statistic "Incomplete Branches" incomplete_branches;
    Print.print_statistic "Restarts" restarts;
    Print.print_statistic "Bound" bound;

end


let create ~(inc: bool) (config: config) : bound =
  (new bound_generic ~inc:inc config :> bound)








class bound_BS (init_bound: int) (config: config) =
object (_self)

  initializer
    ();

  val mutable bound: complexity = init_bound;
  val mutable restarts: int = 0;

  (*** access ***)
  method current_bound : complexity = bound
  method dropped_complexity : complexity option = None  
  method dropped_choice_point : choice_point option = None

  (*** complexity ***)
  method get_complexity (_literal: literal) : complexity = 2
  method get_complexity_subst (_literal: literal) (_subst: subst) (_offset: int) : complexity = 2
  method compare_complexity (_complexity1: complexity) (_complexity2: complexity) : int = 0

  (*** check ***)
  method exceeds (_complexity: complexity) : bool = false
  method exceeds_current (_complexity: complexity) (_choice_point: choice_point) : bool = false
  method register (_complexity: complexity) (_choice_point: choice_point) : bool = false
  method register_subst (_literal: literal) (_subst: subst) (_offset: int) (_choice_point: choice_point) : bool = false
  method has_min_exceeding : bool = false

  (*** backtracking ***)
  method backtrack : unit = ()
  method restart ~(keep_bound: bool) : unit =
    if not keep_bound then begin
      (* for finite domain always increase bound only by 1 *)
      if Config.finite_domain config then begin
        bound <- bound + 1;
      end

      else begin
	failwith "Bound.bound_BS.restart"
      end
    end;

    restarts <- restarts + 1;


  (*** incompleteness ***)
  method is_derivation_incomplete : bool = false
  method incomplete : unit = failwith "Bound.bound_BS.incomplete"

  (*** representation ***)
  method complexity_to_string (complexity: complexity) : string = string_of_int complexity

  method print_statistic : unit =
    Print.print_statistic "Incomplete Branches" 0;
    Print.print_statistic "Restarts" restarts;
    Print.print_statistic "Bound" bound;

end



let create_BS (init_bound: int) (config: config) : bound =
  (new bound_BS init_bound config :> bound)
