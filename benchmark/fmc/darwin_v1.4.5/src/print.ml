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



(* hard coded constant taken based on the current lables
   used in flags and statistic. *)
let label_width = 40

(* hard coded constant. *)
let stats_width = 8


let right_pad string width =
  let l =
    String.length string
  in
    if l >= width then
      string
    else
      string ^ String.make (width - l) ' '

let left_pad string width =
  let l =
    String.length string
  in
    if l >= width then
      string
    else
      String.make (width - l) ' ' ^ string


let convert_float (f: float) : string =
  (* ...x.yz *)
  Printf.sprintf "%.1f" f

let print_label label value =
  print_endline (right_pad label label_width ^ ": " ^ value)

let print_statistic label value =
  print_endline (right_pad label label_width ^ ": " ^ (left_pad (string_of_int value) stats_width))

let print_statistic_64 label value =
  print_endline (right_pad label label_width ^ ": " ^ (left_pad (Int64.to_string value) stats_width))

let print_statistic_float label value =
  print_endline (right_pad label label_width ^ ": " ^ (left_pad (convert_float value) stats_width))


let output_line out string =
  output_string out (string ^ "\n")
