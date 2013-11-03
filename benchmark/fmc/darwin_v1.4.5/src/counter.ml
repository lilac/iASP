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


(* a counter is just an int that is repeatedly incremented by 1.
   only zero and positive values are allowed,
   negative values mark a counter overflow. *)

type counter =
    int ref


exception OVERFLOW


let value (counter: counter) : int =
  !counter



let create_with (start: int) : counter =
  if start >= 0 then
    ref start
  else
    raise OVERFLOW

let create () : counter =
  create_with 0


let set (counter: counter) (value: int) : unit =
  if value >= 0 then
    counter := value
  else
    raise OVERFLOW


let inc_by (counter: counter) (increment: int) : unit =
  let new_value =
    !counter + increment
  in
    if new_value > !counter then
      counter := new_value
    else
      raise OVERFLOW

let inc (counter: counter) : unit =
  inc_by counter 1

let dec (counter: counter) : unit =
  let new_value =
    !counter - 1
  in
    if new_value < !counter then
      counter := new_value
    else
      raise OVERFLOW

let next (counter: counter) : int =
  inc counter;
  value counter
