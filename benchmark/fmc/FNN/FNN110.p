include('Axioms/SWB002+0.ax').
fof(testcase_premise_fullish_011_Entity_Types_as_Classes,axiom,
    ( iext(uri_rdf_type,uri_ex_x,uri_owl_Class)
    & iext(uri_rdf_type,uri_ex_x,uri_owl_ObjectProperty)
    & iext(uri_owl_disjointWith,uri_owl_Class,uri_owl_ObjectProperty) )).

