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



type literal = Term.literal
type config = Config.config
type state = State.state
type choice_point = State.choice_point
type context = Context.context
type raw_context_unifier = Context_unifier.raw_context_unifier
type selected = Selection_types.selected




(*** public interface ***)

class type log =
object
  method apply_assert: selected -> unit
  method apply_split_unit: selected -> unit
  method apply_split_left: choice_point -> selected -> unit
  method apply_split_right: selected -> unit
  method close: raw_context_unifier -> unit
  method incomplete: unit
  method jump: unit
  method backtrack: unit

  method finalize: unit
end








(*** log set ***)

(* contains a set of logs and simply delegates each call to all logs. *)

class log_set
  (protocols: log list) =

object (self)
  val _protocols = protocols

  method iter func =
    List.iter func _protocols

  method exists func =
    List.exists func _protocols

  method apply_assert selected =
    self#iter
      (fun protocol -> protocol#apply_assert selected)

  method apply_split_unit selected =
    self#iter
      (fun protocol -> protocol#apply_split_unit selected)

  method apply_split_left choice_point selected =
    self#iter
      (fun protocol -> protocol#apply_split_left choice_point selected)

  method apply_split_right selected =
    self#iter
      (fun protocol -> protocol#apply_split_right selected)

  method close raw_context_unifier =
    self#iter
      (fun protocol -> protocol#close raw_context_unifier)

  method incomplete =
    self#iter
      (fun protocol -> protocol#incomplete)

  method jump =
    self#iter
      (fun protocol -> protocol#jump)

  method backtrack =
    self#iter
      (fun protocol -> protocol#backtrack)


  method finalize =
    self#iter
      (fun protocol -> protocol#finalize)

end





(*** print ***)

(* textual representation of each rule. *)

(* extended: print the context unifier for any rule *)
class print ~(extended: bool) (config: config) (_state: state) (context: context)
   (out: out_channel) =
object (self)

  method private print id string =
    output_string out (State.id_to_string id ^ string ^ "\n")

  method private print_context_unifier context_unifier =
    if extended then
      output_string out (
	Context_unifier.raw_context_unifier_to_string context_unifier
      )

  method private print_remainder context_unifier =
    if extended then
      output_string out (
	Selection_split.remainder_literals_to_string
	  config context context_unifier
      )

  method private print_apply id label selected =
    self#print id (label ^ ": " ^ Term.literal_to_string selected.Selection_types.literal);
    self#print_context_unifier selected.Selection_types.raw_context_unifier;

  method apply_assert id selected =
    self#print_apply id "Assert" selected;
    
  method apply_split_unit id selected =
    self#print_apply id "Unit Split" selected;
    self#print_remainder selected.Selection_types.raw_context_unifier;

  method apply_split_left id selected =
    self#print_apply id "Split Left" selected;
    self#print_remainder selected.Selection_types.raw_context_unifier;

  method apply_split_right id selected =
    self#print_apply id "Split Right" selected;
    self#print_remainder selected.Selection_types.raw_context_unifier;
    
  method close id (raw_context_unifier: raw_context_unifier) =
    self#print id ("Close");
    self#print_context_unifier raw_context_unifier;

  method incomplete id =
    self#print id ("Bound Reached")

  method jump id =
    self#print id ("Jump")


  method backtrack id backtracked_id =
    self#print id ("Backtracking to: " ^ State.id_to_string backtracked_id)
end













(*** log - pipe ****)

(* immediately write each derivation step *)

type pipe_node = {
  (* a choice point *)
  pi_choice_point: choice_point;

  (* the currently assigned id. *)
  mutable pi_id: int;
}

class pipe (print: print) (state: State.state) =
object (self)
  (* the current derivation branch *)
  val mutable _branch: pipe_node list =
    let root =
      {
	pi_choice_point = State.root_choice_point state;
	pi_id = 0;
      }
    in
      [ root ]

  method current =
    match _branch with
      | [] -> failwith "Protocol.pipe.current"
      | current :: _ -> current
    
  method id =
    self#current.pi_id



  method apply_assert selected =
    print#apply_assert self#id selected
      
  method apply_split_unit selected =
    print#apply_split_unit self#id selected

  method apply_split_left choice_point selected =
    let node =
      {
	pi_choice_point = choice_point;
	pi_id = self#id + 1;
      }
    in
      _branch <- node :: _branch;
      print#apply_split_left node.pi_id selected

  method apply_split_right selected =
    print#apply_split_right self#id selected

  (* sync internal derivation branch to the actual current one. *)
  method backtrack =
    let choice_point =
      State.active_choice_point state
    in
    let rec backtrack' branch =
      match branch with
	| [] ->
	    failwith "log.backtrack 1"

	| head :: tail ->
	    if State.choice_point_equal choice_point head.pi_choice_point then begin
	      print#backtrack self#id head.pi_id;
	      _branch <- branch
	    end
	    else
	      backtrack' tail
    in
      backtrack' _branch;


  method close (raw_context_unifier: raw_context_unifier) =
    print#close self#id raw_context_unifier;
      
  method incomplete =
    print#incomplete self#id;

  method jump =
    print#incomplete self#id;


  method finalize =
    ()
end








(***  creation ***)



let create (config: config) (state: state) (context: context) : log =

  let extended =
    Config.print_derivation_context_unifier config
  in

  let logs =
    []
  in

  let logs =
    if Config.print_derivation_online config then
      (new pipe (new print ~extended:extended config state context stdout) state :> log) :: logs
    else
      logs
  in

  let protocol_set =
    new log_set logs
  in
    (protocol_set :> log)
