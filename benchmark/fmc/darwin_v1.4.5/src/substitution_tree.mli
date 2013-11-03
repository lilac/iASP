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


(** term indexing with a substitution tree

    Subtype of {!Term_indexing}.
    
    A substitution tree stores only substitutions.
    Indexing is achieved by building a tree
    whose nodes are generalizations over similar terms,
    and whose edges are substitutions instantiating towards the terms.
    
    E.g.:
    
    [f(a, g(a, x), g(y, y))] and [f(b, g(b, b), g(x, y))]
    
    can be abstracted to the node [f(x, g(x, y), g(z, u))]
    with the edges [x -> a; u -> z]
    and [x -> b; y -> b].

    (See P. Graf, {{:http://citeseer.ist.psu.edu/graf94substitution.html}Substitution Tree Indexing})
*)


(** {6 Types} *)


type term = Term.term
type literal = Term.literal
type subst = Subst.subst
type 'a data = 'a Term_indexing.data
type 'a predicate_index = 'a Term_indexing.predicate_index
type 'a index = 'a Term_indexing.index


(** {6 Functions} *)

(** creates a {!Term_indexing.predicate_index} using substitution trees. *)
val create_predicate_index: 'data data -> 'data predicate_index

(** creates a {!Term_indexing.index} using substitution trees. *)
val create_index: 'data data -> 'data index
