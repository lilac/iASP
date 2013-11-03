include('Axioms/SWB002+0.ax').
fof(testcase_conclusion_fullish_005_Everything_is_a_Resource,conjecture,
    ( iext(uri_rdf_type,uri_ex_s,uri_owl_Thing)
    & iext(uri_rdf_type,uri_ex_p,uri_rdfs_Resource)
    & iext(uri_rdf_type,uri_ex_p,uri_owl_ObjectProperty)
    & iext(uri_rdf_type,uri_ex_o,uri_rdfs_Resource)
    & iext(uri_rdf_type,uri_ex_o,uri_owl_Thing)
    & iext(uri_rdf_type,uri_ex_p,uri_rdf_Property)
    & iext(uri_rdf_type,uri_ex_p,uri_owl_Thing)
    & iext(uri_rdf_type,uri_ex_s,uri_rdfs_Resource) )).

fof(testcase_premise_fullish_005_Everything_is_a_Resource,axiom,(
    iext(uri_ex_p,uri_ex_s,uri_ex_o) )).

