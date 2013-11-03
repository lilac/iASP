(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2004, 2005
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


exception OVERFLOW

type 'data stack = {
  mutable stack: 'data array;

  (* number of elements in the array,
     i.e. all elements of the array at index >= length are invalid. *)
  mutable size: int;

  (* null element used to fill empty array fields. *)
  null_element: 'data;
}


(* double the array length if the current array is full *)
let inc_size (stack: 'data stack) : unit =
  if stack.size = Array.length stack.stack then begin
    (* already set to max. system array length during the last pass? *)
    if stack.size = Sys.max_array_length then begin
      raise OVERFLOW;
    end;

    let new_size =
      (* resize to at least size 16 *)
      let desired_size = 
	Tools.max_int 16 (2 * (Array.length stack.stack))
      in
	(* obey the hard limit of the max. array size *)
	Pervasives.min Sys.max_array_length desired_size
    in
    let new_array =
      Array.make new_size stack.null_element
    in
      Array.blit stack.stack 0 new_array 0 stack.size;
      stack.stack <- new_array;
  end

let shrink (stack: 'data stack) : unit =
  if stack.size < Array.length stack.stack then begin
    let new_array =
      Array.make stack.size stack.null_element
    in
      Array.blit stack.stack 0 new_array 0 stack.size;
      stack.stack <- new_array;
  end
    


let create (null_element: 'data) : 'data stack = {
  stack = [| |];
  size = 0;
  null_element = null_element;
}

let clear (stack: 'data stack) : unit =
  stack.stack <- [| |];
  stack.size <- 0



let is_empty (stack: 'data stack) : bool =
  stack.size = 0

let size (stack: 'data stack) : int =
  stack.size



let push (stack: 'data stack) (data: 'data) : unit =
  inc_size stack;
  stack.stack.(stack.size) <- data;
  stack.size <- stack.size + 1

let top (stack: 'data stack) : 'data =
  if stack.size = 0 then
    raise Not_found;

  stack.stack.(stack.size - 1)

let pop (stack: 'data stack) : 'data =
  if stack.size = 0 then
    raise Not_found;

  let element =
    stack.stack.(stack.size - 1)
  in
    stack.size <- stack.size - 1;
    stack.stack.(stack.size) <- stack.null_element;
    element

let remove_top (stack: 'data stack) : unit =
  if stack.size = 0 then
    raise Not_found;

  stack.size <- stack.size - 1;
  stack.stack.(stack.size) <- stack.null_element



let iter (func: 'data -> unit) (stack: 'data stack) : unit =
  for i = 0 to stack.size - 1 do
    func stack.stack.(i)
  done
    
let fold (func: 'acc -> 'data -> 'acc) (acc: 'acc) (stack: 'data stack) : 'acc =
  let rec fold' acc' i =
    if i >= stack.size then
      acc'

    else
      fold' (func acc' stack.stack.(i)) (i + 1)
  in
    fold' acc 0


let iter_stop (apply: 'data -> unit) (stop: 'data -> bool) (stack: 'data stack) : unit =
  try
    for i = 0 to stack.size - 1 do
      if stop stack.stack.(i) then
	raise Exit
      else
	apply stack.stack.(i)
    done
  with
    | Exit ->
	()

let map (func: 'data -> 'data) (stack: 'data stack) : 'data stack =
  { stack with
      stack = Array.map func stack.stack;
  } 


let sort (func: 'data -> 'data -> int) (stack: 'data stack) : unit =
  shrink stack;
  Array.sort func stack.stack



let find (stack: 'data stack) (func: 'data -> int) : 'data =
  if stack.size = 0 then
    raise Not_found
  else

    (* binary search on array *)
    let rec exists' (left: int) (right : int) =
      let mid =
	(left + right) / 2
      in
      let cmp =
	func stack.stack.(mid)
      in
	
	if cmp = 0 then
	  stack.stack.(mid)
	    
	else if left >= right then
	  raise Not_found
	    
	else if cmp < 0 then
	  exists' left (mid - 1)
	    
	else (*if cmp > 0 then*)
	  exists' (mid + 1) right
    in
      exists' 0 (stack.size - 1)
