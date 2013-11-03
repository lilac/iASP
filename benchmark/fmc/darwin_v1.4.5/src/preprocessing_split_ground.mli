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

(** splitting of variable disjoint clause parts

    Implements ground splitting, see the paper
    Splitting without Backtracking
    by Alexander Riazanov and Andrei Voronkov.

    Replaces a clause C \/ D,
    where C and D are variable disjoint and not ground,
    with the three clauses C \/ -p, D \/ -q, p \/ q,
    where p and q are fresh 0-ary predicates (of type{!Symbol.sort}.[Connection]).

    Created clauses labelled by such a p can be reused.
    E.g. C \/ D,
    where D' \/ -p and p \/ q do already exists and D and D' are variants,
    is replaced by the clause C \/ -q.
*)

(** {6 Types} *)

type clause = Term.clause


(** {6 Functions} *)

(** performs ground clause splitting and returns the split clauses.

    [print] prints splits.

    a clause is split as often as possible.

    Horn clauses are split into Horn clauses.
*)
val split: print:bool -> clause list -> clause list