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


(** splitting of clause parts

    Implements non-ground splitting, see the paper
    New Techniques that Improve MACE-style Finite Model Finding
    by
    Koen Classen and Niklas Soerensson.

    Say var(C) are the variables contained in the literals C.
    The non-ground splitting replaces a clause C \/ D
    with the clauses C \/ p, D \/ -p.
    var(C) and var(D) must share some variables X,
    but each must alsto contain variables not in X.
    p is a fresh predicate (of type{!Symbol.sort}[.Connection]) over X.
    For example, if X = \{ x, y, z \} then p is p(x, y, z).

    Labelling is used as described in {!Preprocessing_split_ground}.
*)

(** {6 Types} *)

type clause = Term.clause



(** {6 Functions} *)

(** performs non-ground clause splitting and returns the split clauses.

    [print] prints splits.

    a clause is split as often as possible.

    Horn clauses are split into Horn clauses.
*)
val split: print:bool -> clause list -> clause list
