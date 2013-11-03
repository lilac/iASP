include('Axioms/SWB002+0.ax').
fof(testcase_conclusion_fullish_032_Datatype_Relationships,conjecture,
    ( iext(uri_owl_disjointWith,uri_xsd_decimal,uri_xsd_string)
    & iext(uri_rdfs_subClassOf,uri_xsd_integer,uri_xsd_decimal) )).

