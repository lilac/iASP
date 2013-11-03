(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2006
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

(** wrapper for reading zipped files

    during compile time this file or [wrapper_enabled/Zip_wrapper] is chosen,
    depending on if the
    {{:http://caml.inria.fr/cgi-bin/hump.en.cgi?contrib=84}camlzip} library is installed or not.

    if not, this module is chosen, which is just an empty stub.
    then the program compiles, but zip support is not available.
    all functions will fail with the exception NO_ZIP_SUPPORT.
*)


(** {6 Types} *)

type in_file
type entry

exception NO_ZIP_SUPPORT



(** {6 Functions} *)

(** false, as zip support is not available. *)
val enabled: bool

(** @raise NO_ZIP_SUPPORT when called *)
val open_in: string -> in_file

(** @raise NO_ZIP_SUPPORT when called *)
val find_entry: in_file -> string -> entry

(** @raise NO_ZIP_SUPPORT when called *)
val read_entry: in_file -> entry -> string
