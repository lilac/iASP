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



module IntTable =
  Hashtbl.Make (
    struct
      type t = int
	  
      let equal x y : bool =
	x == y
	  
      let hash x : int =
	x
		
    end
  )


module StringTable =
  Hashtbl.Make (
    struct
      type t = string
	  
      let equal (x: string) (y: string) : bool =
	x = y
	  
      let hash (s: string) : int =
	Hashtbl.hash s
		
    end
  )



let list_remove_first (_predicate: 'a -> bool) (_list: 'a list) : 'a list =

  let rec do_list_remove_first (list: 'a list) : 'a list =
    match list with
      | [] ->
	  (* no element has been removed, so reuse the old list *)
	  raise Exit
	    
      | head :: tail ->
	  if _predicate head then
	    tail
	  else
	    head :: do_list_remove_first tail
  in

    try
      do_list_remove_first _list
    with
      | Exit ->
	  _list
	    

let lists_unordered_equal (_equal: 'a -> 'b -> bool) (_list1: 'a list) (_list2: 'b list) : bool =

  let rec do_lists_unordered_equal (list1: 'a list) (list2: 'b list) : bool =
    match list1 with
      | [] ->
	  true

      | head :: tail ->
	  let list2_remainder =
	    list_remove_first (_equal head) list2
	  in
	    if list2 == list2_remainder then
	      false
	    else
	      do_lists_unordered_equal tail list2_remainder
  in

    if List.length _list1 <> List.length _list2 then begin
      false
    end
    else begin
      do_lists_unordered_equal _list1 _list2
    end

let rec lists_equal (equal: 'a -> 'b -> bool) (list1: 'a list) (list2: 'b list) : bool =
  match list1, list2 with
    | [], [] ->
	true

    | head1 :: tail1, head2 :: tail2 when
	equal head1 head2 ->
	lists_equal equal tail1 tail2

    | _ ->
	false



let rec lists_merge (equal: 'a -> 'a -> bool) (list1: 'a list) (list2: 'a list) : 'a list =
  match list2 with
    | [] ->
	list1

    | head :: tail ->
	if List.exists 
	  (fun element ->
	     equal head element
	  )
	  list1
	then
	  lists_merge equal list1 tail
	else
	  head :: lists_merge equal list1 tail

let rec lists_merge (equal: 'a -> 'a -> bool) (list1: 'a list) (list2: 'a list) : 'a list =
  match list2 with
    | [] ->
	list1

    | head :: tail ->
	if List.exists 
	  (fun element ->
	     equal head element
	  )
	  list1
	then
	  lists_merge equal list1 tail
	else
	  head :: lists_merge equal list1 tail


let list_add (equal: 'a -> 'a -> bool) (list: 'a list) (element: 'a) : 'a list =
  if List.exists (equal element) list then
    list
  else
    element :: list


let rec lists_sorted_merge compare list1 list2 =
  match list1, list2 with
    | [], _ -> list2
    | _, [] -> list1
    | h1 :: t1, h2 :: t2 ->
	let cmp =
	  compare h1 h2
	in
	  if cmp == 0 then
	    h1 :: lists_sorted_merge compare t1 t2
	  else if cmp == -1 then
	    h1 :: lists_sorted_merge compare t1 list2
	  else
	    h2 :: lists_sorted_merge compare list1 t2


let list_sorted_add compare list element =
  let rec list_sorted_add' list' =
    match list' with
      | [] ->
	  element :: []
	    
      | h :: t ->
	  let cmp =
	    compare element h
	  in
	    if cmp == 0 then
	      raise Exit
	    else if cmp == -1 then
	      element :: list'
	    else
	      h :: list_sorted_add' t
  in
    try
      list_sorted_add' list
    with
      | Exit ->
	  list


let rec list_sorted_contains compare list element =
  match list with
    | [] ->
	false
    | h :: t ->
	let cmp =
	  compare element h
	in
	  if cmp == 0 then
	    true
	  else if cmp == -1 then
	    false
	  else
	    list_sorted_contains compare t element


let rec list_first (number: int) (list: 'a list) : 'a list =
  match list with
    | [] ->
	[]

    | head :: tail ->
	if number = 0 then
	  []
	else
	  head :: list_first (number - 1) tail


let do_lists_intersect (equal: 'a -> 'a -> bool) (list1: 'a list) (list2: 'a list) : bool =
  List.exists
    (fun element1 ->
       (List.exists
	  (fun element2 -> 
	     equal element1 element2
	  )
	  list2
       )
    )
    list1




let lists_shared (_equal: 'a -> 'a -> bool) (_lists: 'a list list) : 'a list =
  
  let rec do_lists_shared (shared: 'a list) (already_occured: 'a list) (lists: 'a list list) :
    'a list =
    
    match lists with
      | [] ->
	  shared
	  
      | head :: tail ->
	  let (new_shared, new_already_occured) =
	    List.fold_left
	      (fun (fold_shared, fold_already_occured) element ->
		 if List.exists (_equal element) fold_already_occured then begin
		   if List.exists (_equal element) fold_shared then
		     (fold_shared, fold_already_occured)
		   else
		     (element :: fold_shared, fold_already_occured)
		 end
		 else
		   (fold_shared, element :: fold_already_occured)
	      )
	      (shared, already_occured)
	      head
	  in
	    do_lists_shared new_shared new_already_occured tail
  in
    do_lists_shared [] [] _lists
      


let list_map (_func: 'a -> 'a) (_list: 'a list) : 'a list =

  let rec list_map' list =
    match list with
      | [] ->
	  []
	    
      | head :: tail ->
	  let tail' =
	    list_map' tail
	  and head' =
	    _func head
	  in
	    if tail == tail'
	       &&
	       head == head'
	    then
	      list
	    else
	      head' :: tail'
	    
  in
    list_map' _list


let list_map_find (p: 'a -> bool) (f: 'a -> 'b) (l: 'a list) : ('b list) =
  
  let rec list_map_find' l' =
    match l' with
    | [] -> []

    | h :: t ->
	if p h then
	  f h :: list_map_find' t
	else
	  list_map_find' t
  in

  list_map_find' l


let mapping_extend (_mapping: ('a * 'b) list) (_key_equal: 'a -> 'a -> bool) (_value_equal: 'b -> 'b -> bool)
  (_key: 'a) (_value: 'b): ('a * 'b) list =
  
  let rec do_mapping_extend (mapping: ('a * 'b) list) =
    match mapping with
      | [] ->
	  (_key, _value) :: _mapping

      | (key, value) :: tail ->
	  if
	    (_key_equal _key key)
	    &&
	    (_value_equal _value value)
	  then
	    (* mapping already contains the old entry *)
	    _mapping
	  else if
	    (_key_equal _key key)
	    ||
	    (_value_equal _value value)
	  then
	    raise Exit
	  else
	    do_mapping_extend tail
  in

    do_mapping_extend _mapping








let array_iter2 (_func: 'a -> 'b -> unit) (_array1: 'a array) (_array2: 'b array) : unit =
  if Array.length _array1 <> Array.length _array2 then begin
    raise Exit
  end
  else begin
    for i = 0 to Array.length _array1 - 1 do
      _func _array1.(i) _array2.(i)
    done;
  end



let array_fold2 (_func: 'a -> 'b -> 'c -> 'a) (_acc: 'a) (_array1: 'b array) (_array2: 'c array) : 'a =

  let rec fold_at (index: int) (acc: 'a) : 'a =
    if index >= Array.length _array1 then begin
      acc
    end
    else begin
      let new_acc =
	_func acc _array1.(index) _array2.(index)
      in
	fold_at (index + 1) new_acc
    end
  in


    if Array.length _array1 <> Array.length _array2 then begin
      raise Exit
    end
    else begin
      fold_at 0 _acc
    end



let array_exists (_predicate: 'a -> bool) (_array: 'a array) : bool =

  let rec do_array_exists (index: int) : bool =
    if index >= Array.length _array then
      false
    else if _predicate _array.(index) then
      true
    else
      do_array_exists (index + 1)
  in

    do_array_exists 0
  
let array_for_all (_predicate: 'a -> bool) (_array: 'a array) : bool =
  not (
    array_exists (fun x -> not (_predicate x)) _array
  )


let array_find (_predicate: 'a -> bool) (_array: 'a array) : 'a =

  let rec do_array_exists (index: int) : 'a =
    if index >= Array.length _array then
      raise Not_found
    else if _predicate _array.(index) then
      _array.(index)
    else
      do_array_exists (index + 1)
  in

    do_array_exists 0


let array_for_all2 (_func: 'a -> 'b -> bool) (_array1: 'a array) (_array2: 'b array) : bool =

  let rec do_at (index: int) : bool =
    if index >= Array.length _array1 then begin
      true
    end
    else begin
      _func _array1.(index) _array2.(index)
      &&
      do_at (index + 1)
    end
  in
    Array.length _array1 == Array.length _array2
    &&
    do_at 0



let rec compare_lists x y =
  match x, y with
    | [], [] ->
	1

    | [], _ ->
	-1

    | _, [] ->
	1

    | _ :: x, _ :: y ->
	compare_lists x y



let compare_int (x: int) (y: int) : int =
  if x == y then
    0
  else if x < y then
    -1
  else
    1


let max_int (x: int) (y: int) : int =
  if x > y then
    x
  else
    y





let find_max (cmp: 'a -> 'a -> int) (list: 'a list) : 'a =
  match list with
    | [] ->
	raise Not_found

    | head :: tail ->
	List.fold_left
	  (fun max next ->
	     if cmp max next >= 0 then
	       max
	     else
	       next
	  )
	  head
	  tail



let lists_sorted_merge_int = lists_sorted_merge compare_int

let list_sorted_add_int = list_sorted_add compare_int

let list_sorted_contains_int = list_sorted_contains compare_int
