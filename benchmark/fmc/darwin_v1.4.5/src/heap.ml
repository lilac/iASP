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



module type OrderedType = sig

  type t

  val compare: t -> t -> int

  val to_string: t -> string
end




(* A heap is a binary tree with the invariants that
   - the minimum element is always on top,
   - and that the children of a node are always greater than the node.

   For efficiency an array instead of a tree is used in the implementation.
   Then the children of the element i are at 2i + 1 and 2i + 2.
*)
module Heap (Ord: OrderedType) = struct

  (*** types ***)
  type data = Ord.t

  type heap = {
    (* the heap *)
    mutable hp_heap: data array;

    (* number of elements in the heap,
       i.e. all elements of the array at index >= hp_length are invalid. *)
    mutable hp_length: int;

    (* null element used to fill empty array fields.
       avoids keeping them filled with removed older entries,
       so that those can be gargabe collected. *)
    hp_null_element: data;
  }

  type t = heap


  (*** creation ***)

  let create (null_element: data) : heap = {
    hp_heap = [| |];
    hp_length = 0;
    hp_null_element = null_element;
  }



  (*** access ***)

  let min (heap: heap) : data = 
    if heap.hp_length <= 0 then begin
      raise Not_found
    end;

    heap.hp_heap.(0)


  (*** check ***)

  (* check that the heap ensures the invariant,
     i.e. has a proper structure and will return the minimum element *)
  let check (heap : heap) : unit =
    if Const.debug then begin
      for i = 0 to heap.hp_length - 1 do
	let left =
	  (2 * i) + 1
	in
	let right =
	(2 * i) + 2
	in
	  if left < heap.hp_length then begin
	    if Ord.compare heap.hp_heap.(left) heap.hp_heap.(i) < 0 then begin
	      failwith "Heap.check 1";
	    end
	  end;
	  
	  if right < heap.hp_length then begin
	    if Ord.compare heap.hp_heap.(left) heap.hp_heap.(i) < 0 then begin
	      failwith "Heap.check 2";
	    end
	  end;
      done
    end



  (*** add ***)


  (* double the array length if it's exceeded *)
  let increase_heap_size (heap: heap) (data: data) : unit =
    if heap.hp_length >= Array.length heap.hp_heap then begin
      (* already set to max length during the last pass? *)
      if heap.hp_length = Sys.max_array_length then begin
	raise OVERFLOW;
      end;

      let new_size =
	let desired_size = 
	  Tools.max_int 256 (2 * (Array.length heap.hp_heap))
	in
	  (* obey the hard limit of the max. array size *)
	  Pervasives.min Sys.max_array_length desired_size
      in
	let new_heap =
	  Array.make new_size data
	in
	  Array.blit heap.hp_heap 0 new_heap 0 (Array.length heap.hp_heap);
	  heap.hp_heap <- new_heap;
    end


  (* a new element is added at the end.
     switch it with its parent element if it is smaller *)
  let rec sift_up (heap: heap) (data: data) (index: int) : unit =
    if index = 0 then begin
      heap.hp_heap.(index) <- data;
    end

    else begin
      let parent_index =
	(index - 1) / 2
      in
	if Ord.compare data heap.hp_heap.(parent_index) < 0 then begin
	  heap.hp_heap.(index) <- heap.hp_heap.(parent_index);
	  sift_up heap data parent_index
	end

	else begin
	  heap.hp_heap.(index) <- data;
	end
    end


  let add (heap: heap) (data: data) : unit =
    increase_heap_size heap data;

    sift_up heap data heap.hp_length;

    heap.hp_length <- heap.hp_length + 1;
    
    check heap


  (*** remove ***)

  (* the first element has been retrieved.
     so remove the last element, put it in front,
     and go down the tree while the last element is greater
     than the current node's children, switching it with the child.
     if it's smaller than the children, insert it *)
  let rec sift_down (heap: heap) (data: data) (index: int) : unit =
    let left_child_index =
      index * 2 + 1
    in
      (* at the bottom of the heap, so insert here *)
      if left_child_index > heap.hp_length then begin
	heap.hp_heap.(index) <- data;
      end
	  
      else begin
	let right_child_index =
	  index * 2 + 2
	in
	    
	(* which of the two child nodes is smaller? *)
	let smaller_child_index =
	  if right_child_index > heap.hp_length then begin
	    left_child_index
	  end
	    
	  else begin
	    if Ord.compare heap.hp_heap.(left_child_index) heap.hp_heap.(right_child_index) <= 0 then
	      left_child_index
	    else
	      right_child_index
	  end
	in
	  (* is data smaller than the smallest child? *)
	  if Ord.compare data heap.hp_heap.(smaller_child_index) <= 0 then begin
	    (* yes, so insert it here *)
	    heap.hp_heap.(index) <- data;
	  end

	  else begin
	    (* no, so replace with the smaller child and descend in its branch *)
	    heap.hp_heap.(index) <- heap.hp_heap.(smaller_child_index);
	    sift_down heap data smaller_child_index;
	  end
	end


  let remove_min (heap: heap) : data = 
    if heap.hp_length <= 0 then begin
      raise Not_found
    end;

    let data =
      heap.hp_heap.(0)
    in
      heap.hp_length <- heap.hp_length - 1;
      
      if heap.hp_length > 0 then begin
	sift_down heap heap.hp_heap.(heap.hp_length) 0
      end;

      heap.hp_heap.(heap.hp_length) <- heap.hp_null_element;

      check heap;

      data
    


  (*** iteration ***)

  let iter (func: data -> unit) (heap: heap) : unit =
    for i = 0 to heap.hp_length - 1 do
      func heap.hp_heap.(i)
    done



  (*** size ***)
  let is_empty (heap: heap) : bool =
    heap.hp_length = 0
    

  let size (heap: heap) : int =
    heap.hp_length

end







(* A min-max heap is a binary tree with the invariants that
   - the minimum element is always on top,
   - the maximum element is one of its children,
   - and values stored at even (odd) levels are smaller (greater) than
     values stored at their children.
   Even (odd) levels are called min (max) levels.

   For efficiency an array instead of a tree is used in the implementation.
   Then the children of the element i are at 2i + 1 and 2i + 2.
*)
module MinMaxHeap (Ord: OrderedType) = struct

  (*** types ***)
  type data = Ord.t

  type heap = {
    (* the heap *)
    mutable hp_heap: data array;

    (* number of elements in the heap,
       i.e. all elements of the array at index >= hp_length are invalid. *)
    mutable hp_length: int;

    (* null element used to fill empty array fields.
       avoids keeping them filled with removed older entries,
       so that those can be gargabe collected. *)
    hp_null_element: data;
  }

  type t = heap

  let compare_min =
    Ord.compare

  let compare_max x y =
    Ord.compare y x


  (*** creation ***)

  let create (null_element: data) : heap = {
    hp_heap = [| |];
    hp_length = 0;
    hp_null_element = null_element;
  }


  (*** representation ***)
  let to_string (heap: heap) : string =
    let rec to_string' i =
      if i >= heap.hp_length then
	""
      else
	string_of_int i ^ ": " ^ Ord.to_string heap.hp_heap.(i) ^ "\n" ^ to_string' (i + 1)
    in
      to_string' 0



  (*** helper functions ***)

  (* is this a min or a max level? *)
  let is_min_level (index: int) : bool =
    let level =
      log (float_of_int (index + 1)) /. log (float_of_int 2)
    in
      (* even -> min level *)
      ((int_of_float level) mod 2) = 0


  (* get the index of the left child *)
  let left_child_index (index: int) : int =
    index * 2 + 1

  (* get the index of the right child *)
  let right_child_index (index: int) : int =
    index * 2 + 2

  (* get the index of the parent *)
  let parent_index (index: int) : int =
    (index - 1) / 2

  (* is this index part of the heap? *)
  let within_bound (heap: heap) (index: int) : bool =
    index < heap.hp_length



  (* get the smaller of two elements according to the given comparison function *)
  let min_entry (compare: data -> data -> int) (heap: heap) (first_index: int) (second_index: int) : int =
    if compare heap.hp_heap.(first_index) heap.hp_heap.(second_index) <= 0 then
      first_index
    else
      second_index

  (* get the minimum of the entry and its two children according to the given comparison function *)
  let min_with_descendants (compare: data -> data -> int) (heap: heap) (index: int) : int =
    let left_child =
      left_child_index index
    in
      (* node has children, so check them *)
      if within_bound heap left_child then begin
	let right_child =
	  right_child_index index
	in
	  (* both children exist *)
	  if within_bound heap right_child then
	    min_entry compare heap index (min_entry compare heap left_child right_child)

	  (* only left child exists *)
	  else
	    min_entry compare heap index left_child
      end
	
      (* no children *)
      else
	index
	
      



  (*** add ***)


  (* double the array length if it is full *)
  let increase_heap_size (heap: heap) (data: data) : unit =
    if heap.hp_length >= Array.length heap.hp_heap then begin
      (* already set to max length during the last pass? *)
      if heap.hp_length = Sys.max_array_length then begin
	raise OVERFLOW;
      end;

      let new_size =
	let desired_size = 
	  Tools.max_int 256 (2 * (Array.length heap.hp_heap))
	in
	  (* obey the hard limit of the max. array size *)
	  Pervasives.min Sys.max_array_length desired_size
      in
	let new_heap =
	  Array.make new_size data
	in
	  Array.blit heap.hp_heap 0 new_heap 0 (Array.length heap.hp_heap);
	  heap.hp_heap <- new_heap;
    end



  (* new element has been added at position index,
     move it up until the invariants are fulfilled again.
     
     compare decides if min (compare_min) or max (compare_max) level propagation is done. *)
  let rec sift_up' (compare: data -> data -> int) (heap: heap) (data: data) (index: int) : unit =
    (* grandparent exists? *)
    if index > 2 then begin
      let grand_parent : int =
	parent_index (parent_index index)
      in
	if compare data heap.hp_heap.(grand_parent) < 0 then begin
	  (* swap *)
	  heap.hp_heap.(index) <- heap.hp_heap.(grand_parent);
	  sift_up' compare heap data grand_parent
	end

	else
	  (* stop. *)
	  heap.hp_heap.(index) <- data;
    end

    else begin
      (* stop. *)
      heap.hp_heap.(index) <- data;
    end

  (* new element has been added at position index,
     decided if it has to be pushed up to min or max levels
     in order to meet the invariants again *)
  let rec sift_up (heap: heap) (data: data) (index: int) : unit =
    if is_min_level index then begin
      let parent =
	parent_index index
      in
	if Ord.compare data heap.hp_heap.(parent) > 0 then begin
	  (* min level and greater than parents,
	     so move up on the max levels *)
	  heap.hp_heap.(index) <- heap.hp_heap.(parent);
	  sift_up' compare_max heap data parent
	end

	else begin
	  (* min level and smaller than parents,
	     so move up on the min levels *)
	  sift_up' compare_min heap data index
	end
    end

    else begin
      let parent =
	parent_index index
      in
	if Ord.compare data heap.hp_heap.(parent) < 0 then begin
	  (* max level and smaller than parents,
	     so move up on the min levels *)
	  heap.hp_heap.(index) <- heap.hp_heap.(parent);
	  sift_up' compare_min heap data parent
	end

	else begin
	  (* max level and greater than parents,
	     so move up on the max levels *)
	  sift_up' compare_max heap data index
	end
    end



  let add' (heap: heap) (data: data) : unit =
    increase_heap_size heap data;

    if heap.hp_length = 0 then
      heap.hp_heap.(0) <- data
    else
      sift_up heap data heap.hp_length;

    heap.hp_length <- heap.hp_length + 1



  (*** remove ***)



  (* the minimum or maximum element has been retrieved.
     so remove the last element, put it in the gap,
     and swap it down the heap till the invariants are ok again. *)
  let rec sift_down (compare: data -> data -> int) (heap: heap) (data: data) (index: int) : unit =
    let left_child =
      left_child_index index
    in
      (* node has children, so check them *)
      if within_bound heap left_child then begin
	(* check left subtree first *)
	let min_left_index =
	  min_with_descendants compare heap left_child
	in

	let right_child =
	  right_child_index index
	in
	  
	(* check right subtree next *)
	let min_children_index =
	  (* node has right children, so check them *)
	  if within_bound heap right_child then begin
	    min_entry compare heap min_left_index (min_with_descendants compare heap right_child)
	  end
	    
	  (* no right children *)
	  else
	    min_left_index
	in

	  (* new data is already at the correct position, stop here *)
	  if compare data heap.hp_heap.(min_children_index) <= 0 then begin
	    heap.hp_heap.(index) <- data;
	  end

	  (* the smallest descendant is an immediate child *)
	  else if
	    min_children_index = left_child
	    ||
	    min_children_index = right_child
	  then begin
	    (* child position is the final position, swap and be done *)
	    heap.hp_heap.(index) <- heap.hp_heap.(min_children_index);
	    heap.hp_heap.(min_children_index) <- data;
	  end

	  (* the smallest descendant is a grand child *)
	  else begin
	    (* swap with grand child *)
	    heap.hp_heap.(index) <- heap.hp_heap.(min_children_index);
	    
	    (* if necessary swap with new parent *)
	    let parent_index =
	      parent_index min_children_index
	    in
	      if compare data heap.hp_heap.(parent_index) > 0 then begin
		let new_data =
		  heap.hp_heap.(parent_index)
		in
		  heap.hp_heap.(parent_index) <- data;
		  sift_down compare heap new_data min_children_index
	      end
		
	      else begin
		(* and continue *)
		sift_down compare heap data min_children_index
	      end
	  end
    end

    (* leaf node, stop here *)
    else begin
      heap.hp_heap.(index) <- data;
    end



  (*** access ***)

  let min (heap: heap) : data = 
    if heap.hp_length <= 0 then begin
      raise Not_found
    end;

    heap.hp_heap.(0)

  let remove_min' (heap: heap) : data = 
    let data =
      min heap
    in
      heap.hp_length <- heap.hp_length - 1;
      
      if heap.hp_length > 0 then begin
	sift_down compare_min heap heap.hp_heap.(heap.hp_length) 0
      end;

      heap.hp_heap.(heap.hp_length) <- heap.hp_null_element;

      data
    

  let max_index (heap: heap) : int = 
    if heap.hp_length <= 0 then
      raise Not_found

    else if heap.hp_length = 1 then
      0

    else if heap.hp_length = 2 then
      1

    else if Ord.compare heap.hp_heap.(1) heap.hp_heap.(2) > 0 then
      1

    else
      2


  let max (heap: heap) : data = 
    heap.hp_heap.(max_index heap)


  let remove_max' (heap: heap) : data =
    let index =
      max_index heap
    in

    let data =
      heap.hp_heap.(index)
    in
      heap.hp_length <- heap.hp_length - 1;
      
      
      (* 0 element -> nothing to do
	 1 element -> the minmum element, nothing to do
	 >1 elements ->
	 move the last element to the gap and let move it down to the right position *)
      if heap.hp_length > 1 then begin
	sift_down compare_max heap heap.hp_heap.(heap.hp_length) index
      end;

      heap.hp_heap.(heap.hp_length) <- heap.hp_null_element;

      data


  (*** check ***)

  (* check that the heap meets the invariants *)

  let check (heap : heap) : unit =
    if Const.debug then begin

      (* all element must be valid *)
      for i = 0 to heap.hp_length - 1 do
	if heap.hp_heap.(i) == heap.hp_null_element then
	  failwith "Min_max_heap.check: null_element"
      done;
      
      (* must be smaller than children and grand children for a min level,
	 greater for a max level. *)
      for i = 0 to heap.hp_length - 1 do
	let check_at index =
	  if is_min_level i then begin
	    if
	      within_bound heap index
	      &&
	      Ord.compare heap.hp_heap.(i) heap.hp_heap.(index) > 0
	    then
	      failwith "Min_max_heap.check: min_level"
	  end

	  else begin
	    if
	      within_bound heap index
	      &&
	      Ord.compare heap.hp_heap.(i) heap.hp_heap.(index) < 0
	    then
	      failwith ("Min_max_heap.check: max_level " ^ string_of_int i ^ " <-> " ^ string_of_int index);
	  end
	in
	  check_at (left_child_index i);
	  check_at (left_child_index (left_child_index i));
	  check_at (right_child_index (left_child_index i));
	  check_at (right_child_index i);
	  check_at (left_child_index (right_child_index i));
	  check_at (right_child_index (right_child_index i));
      done;

      (* check that remove_min preserves increasing order *)
      begin
	let heap' = { heap with
			hp_heap = Array.copy heap.hp_heap
		    }
	in
	let last_min =
	  ref None
	in
	  while heap'.hp_length > 0 do
	    let min =
	      remove_min' heap'
	    in
	      begin
		match !last_min with
		  | None ->
		      ()
			
		  | Some last ->
		      if Ord.compare last min >= 0 then begin
			print_endline ("PREV: " ^ Ord.to_string last);
			failwith "Heap.check' min";
		      end
	      end;
	      last_min := Some min
	  done
      end;

      (* check that remove_max preserves decreasing order *)
      begin
	(*    print_newline ();
	      print_endline ("CHECK");
	      print_endline ("ORIG:");
	      print_heap heap to_string;*)
	let heap' = { heap with
			hp_heap = Array.copy heap.hp_heap
		    }
	in
	let last_max =
	  ref None
	in
	  while heap'.hp_length > 0 do
	    let max =
	      remove_max' heap'
	    in
	      (*	  print_endline ("CURRENT:");
			  print_heap heap';
			  print_endline ("MAX: " ^ to_string max);*)
	      begin
		match !last_max with
		  | None ->
		      ()
			
		  | Some last ->
		      if Ord.compare last max <= 0 then begin
			print_endline ("PREV: " ^ Ord.to_string last);
			failwith "Heap.check' max";
		      end
	      end;
	      last_max := Some max
	  done
      end;
    end




  (*** wrapper functions to include integrity checks *)


  let add (heap: heap) (data: data) : unit =
    add' heap data;
    check heap

  let remove_min (heap: heap) : data =
    let min =
      remove_min' heap
    in
      check heap;
      min

  let remove_max (heap: heap) : data =
    let max =
      remove_max' heap
    in
      check heap;
      max



  (*** iteration ***)

  let iter (func: data -> unit) (heap: heap) : unit =
    for i = 0 to heap.hp_length - 1 do
      func heap.hp_heap.(i)
    done



  (*** size ***)
  let is_empty (heap: heap) : bool =
    heap.hp_length = 0
    

  let size (heap: heap) : int =
    heap.hp_length

end

(* some test code
module Test =
  MinMaxHeap (
    struct
      type t = int

      let compare = compare
      let to_string = string_of_int
    end
  )

let (_ : unit) =
  let numbers =
    [1; 2; 3; 4; 5; 6]
  in
  let heap =
    Test.create 0
  in
    List.iter
      (fun number ->
	 Test.add heap number;
	 Test.check' heap;
      )
      numbers;

    failwith "NO ERROR FOUND"

    *)
