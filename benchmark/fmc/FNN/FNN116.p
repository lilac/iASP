include('Axioms/SWB002+0.ax').
fof(testcase_conclusion_fullish_018_Modified_Logical_Vocabulary_Semantics,conjecture,(
    iext(uri_rdf_type,uri_ex_u,uri_ex_Person) )).

fof(testcase_premise_fullish_018_Modified_Logical_Vocabulary_Semantics,axiom,
    ( iext(uri_rdfs_domain,uri_owl_sameAs,uri_ex_Person)
    & iext(uri_owl_sameAs,uri_ex_w,uri_ex_u) )).

