(*
This file is part of the first order theorem prover Darwin
Copyright (C) 2006  The University of Iowa

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


type clause = Term.clause

let to_clauses (channel: Lexing.lexbuf) : (clause list * string list) =
  try
    Parser_tptp.parse_file Lexer_tptp.token channel
  with
  | Parsing.Parse_error ->
      Lexer_tptp.parse_error ()

let to_clauses_from_string (source: string) : clause list =
  let channel = Lexing.from_string source in
  let clauses, includes = to_clauses channel in
  match includes with
  | [] -> clauses
  | _  -> failwith ("Includes in to_clauses_from_string")


let rec to_clauses_from_files (path: string) (files : string list)
    (clauses : clause list) : clause list =
  match files with
  | [] ->
      clauses
  | h :: t ->
      let file_in =
        try
          open_in (Filename.concat path h)
        with
        | Sys_error _ ->
            let tptp_path =
              try
                Sys.getenv "TPTP"
              with
              | Not_found ->
                  path
            in
            open_in (Filename.concat tptp_path h)
      in
      let channel = Lexing.from_channel file_in in
      let clauses', includes' = to_clauses channel in
      close_in file_in;
      to_clauses_from_files path (includes' @ t) (clauses' @ clauses)

let to_clauses_from_file (file_name: string) : clause list =
  let path = Filename.dirname file_name in
  let file = Filename.basename file_name in
  to_clauses_from_files path [file] []
