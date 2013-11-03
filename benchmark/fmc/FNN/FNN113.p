include('Axioms/SWB002+0.ax').
fof(testcase_premise_fullish_007_Equal_Classes,axiom,
    ( iext(uri_rdfs_subClassOf,uri_ex_c,uri_ex_c1)
    & iext(uri_rdfs_range,uri_ex_p,uri_ex_c1)
    & iext(uri_rdf_type,uri_ex_w,uri_ex_c1)
    & iext(uri_owl_sameAs,uri_ex_c1,uri_ex_c2) )).

fof(testcase_conclusion_fullish_007_Equal_Classes,conjecture,
    ( iext(uri_rdfs_subClassOf,uri_ex_c,uri_ex_c2)
    & iext(uri_rdfs_range,uri_ex_p,uri_ex_c2)
    & iext(uri_rdf_type,uri_ex_w,uri_ex_c2) )).

