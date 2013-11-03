(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2005
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



type config = Config.config
type statistic = Statistic.statistic
type literal = Term.literal
type state = State.state
type choice_point = State.choice_point
type bound = Bound.bound


type guiding_step =
  | Left of literal
  | Right of literal
  | Split of literal

type guiding_path =
    guiding_step list



type jumping = {
  config: config;
  mutable state: state;
  bound: bound;
  statistic: statistic;

  (* all recorded and unprocessed jumps *)
  mutable guiding_paths: guiding_path list;

  (* splits since last jump check *)
  split_counter: Counter.counter;
}




(*** creation ***)

let create (config: config) (statistic: statistic) (state: state) (bound: bound) : jumping =
  {
    config = config;
    state = state;
    bound = bound;
    statistic = statistic;
    guiding_paths = [];
    split_counter = Counter.create ();
  }




(*** replay ***)

let finished (jumping: jumping) : bool =
  jumping.guiding_paths == []


let replay (jumping: jumping) (state: state) : guiding_path =
  (* return oldest stored guiding path first *)
  let path =
    match jumping.guiding_paths with
      | [] ->
	  raise Not_found
	    
      | head :: tail ->
	  jumping.guiding_paths <- tail;
	  head
  in
    jumping.state <- state;
    Counter.set jumping.split_counter 0;

    path



(*** representation ***)

let guiding_path_to_string (guiding_path: guiding_path) : string =
  String.concat "\n"
    (List.map
       (fun step ->
	  match step with
	    | Left literal ->
		"LEFT: " ^ Term.literal_to_string literal;
		
	    | Right literal ->
		"RIGHT: " ^ Term.literal_to_string literal;
		
	    | Split literal ->
		"SPLIT: " ^ Term.literal_to_string literal;
       )
       guiding_path
    )



(*** jumping ***)

(* how many percent of the given timeout is left? *)
let get_remaining_time_ratio (config: config) : float =
  if Config.time_out_CPU config = 0.0 then begin
    (* 1.0 *)
    (* should not be called if no timeout is given *)
    failwith "Jumping.get_remaining_time_ratio";
  end
    
  else begin
    (Config.time_out_CPU config -. Sys.time ())
    /.
    (Config.time_out_CPU config)
  end



(* just jump back the minimum distance *)
let find_default_jump_target (jumping: jumping) : choice_point option =
  (* check only 10 times as rarely as in the resource constrainted case *)
  if 
    (
      (Counter.value jumping.split_counter)
      mod
      (Const.jumping_check_every_splits * 10)
    )
    !=
    0
  then begin
    None
  end

  else begin
    let branch =
      State.get_branch jumping.state
    in
      
    let rec find_default_jump_target'
	(remaining: choice_point list) (skipped: int) : choice_point option =
      match remaining with
	| [] ->
	    None
	      
	| head :: tail ->
	    if skipped = Const.jumping_min_distance then
	      Some head
	    else
	      find_default_jump_target' tail (skipped + 1)
    in
      find_default_jump_target' branch 0
  end


(* find the choice point to backjump to so that the remaining search space
   can (potentially) be searched within the remaing time. *)
let find_resource_jump_target (jumping: jumping) : choice_point option =
  let branch =
    List.rev (State.get_branch jumping.state)
  in

  let time_ratio : float =
    get_remaining_time_ratio jumping.config
  in
      
  let time_ratio' =
    time_ratio +. Const.jumping_time_delta
  in

    (* no jumping if basically all time is left *)
    if time_ratio' >= 1.0 then begin
      None
    end

    else  begin
      (* start at root, and go down the current derivation branch.
	 each decision level i counts as a fraction (1 / 2^i) of the search space.
	 so if the sum of these fractions is greater than the remaining time ratio,
	 the remaining search space from this choice point on is too large for the remaining time,
	 and a jump can happen before that choice point.
      *)
      let rec find_resource_jump_target'
	  (space_ratio: float) (depth: int) (remaining_branch: choice_point list) : choice_point option =

	match remaining_branch with
	  | [] ->
	      (* estimation: all remaining space can be searched within the remaining time. *)
	      None
		
	  | decision_level :: tail ->
	      (* ignore the root level *)
	      if State.choice_point_equal decision_level (State.root_choice_point jumping.state) then begin
		find_resource_jump_target' space_ratio (depth + 1) tail
	      end
		
	      else begin
		(* add the search space of the unexplored right split -
		   the search space is organized as a binary tree *)
		let space_ratio' =
		  space_ratio +. (1.0 /. (2.0 ** (float_of_int depth)))
		in
		  
		  (* not enough time left, skip from here on *)
		  if space_ratio' > time_ratio' then begin
		    
		    (* ... but only if the jump is not too short *)
		    if List.length tail <= Const.jumping_min_distance - 1  then
		      None

		    else
		      Some decision_level
		  end
		    
		  (* enough time left, continue search ... *)
		  else begin
		    find_resource_jump_target' space_ratio' (depth + 1) tail
		  end
	      end
      in
	find_resource_jump_target' 0.0 0 branch
    end


(* convert the current path to a guiding_path *)
let compute_guiding_path (state: state) (jump_target: choice_point) : guiding_path =

  let add_right_splits decision_level guiding_path =
    let right_splits =
      List.rev (State.right_split_literals decision_level)
    in
      List.fold_left
	(fun acc split ->
	   Right split :: acc
	)
	guiding_path
	right_splits
  in

  (* start from current level and go backwards to root *)
  let rec compute_guiding_path' (remaining_branch: choice_point list) (guiding_path: guiding_path) : guiding_path =
    match remaining_branch with
      | [] ->
	  guiding_path

      | decision_level :: tail ->
	  (* the root level - provide only right splits *)
	  if State.choice_point_equal decision_level (State.root_choice_point state) then begin
	    compute_guiding_path' tail (add_right_splits decision_level guiding_path)
	  end

	  (* jumped over - replay full, all left and right splits *)
	  else if State.compare_age decision_level jump_target > 0 then begin
	    let split =
	      Split (State.left_split_literal decision_level)
	    in
	      compute_guiding_path'
		tail
		(split :: add_right_splits decision_level guiding_path)
	  end

	  (* no jumped over -
	     this search space will still be covered by the regular search after the jump.
	     so replay all left and right splits as asserts. *)
	  else (*if State.compare_age decision_level jump_target <= 0 then*) begin
	    let split =
	      Left (State.left_split_literal decision_level)
	    in
	      compute_guiding_path'
		tail
		(split :: add_right_splits decision_level guiding_path)
	  end
  in
    compute_guiding_path' (State.get_branch state) []



let jump (jumping: jumping) : choice_point option =
  Counter.inc jumping.split_counter;

  if not (Config.jumping jumping.config) then begin
    None
  end

  (* only check once in a while *)
  else if ((Counter.value jumping.split_counter) mod Const.jumping_check_every_splits) != 0 then begin
    None
  end

  else begin
    let jump_target =
      if Config.time_out_CPU jumping.config = 0.0 then begin
	(* no timeout given, just jump so often *)
	find_default_jump_target jumping
      end
      else begin
	(* jump based on the time left *)
	find_resource_jump_target jumping
      end
    in
      (* jump target found? *)
      match jump_target with
	| None ->
	    (* no, no jump *)
	    None

	| Some jump_target ->
	    (* yes, but if this jump will leave us within an incomplete branch
	       just jump so much farther, that we live the incomplete branch.
	       after all, we want to find a model. *)
	    let jump_target =
	      match jumping.bound#dropped_choice_point with
		| None ->
		    jump_target

		| Some other ->
		    if State.compare_age jump_target other > 0 then
		      other
		    else
		      jump_target
	    in

	    let guiding_path =
	      compute_guiding_path jumping.state jump_target
	    in
	      (* new path to the end *)
	      jumping.guiding_paths <- jumping.guiding_paths @ [guiding_path];
	      
	      Statistic.inc_jump jumping.statistic;
(*	      print_endline (guiding_path_to_string guiding_path);*)
	      
	      Some jump_target
  end
