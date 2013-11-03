include('Axioms/SWV009+0.ax').
fof(ax48,hypothesis,(
    oca_compartment_has_scg(oca,compartmenta,scg_compartmenta) )).

fof(ax41,hypothesis,(
    system_indi_is_oca(system,oca) )).

fof(ax64,hypothesis,(
    system_file_needs_citizenship(system,not_secretfile,anycountry) )).

fof(ax68,hypothesis,(
    system_indi_is_background_admin(system,background_admin) )).

fof(ax43,hypothesis,(
    oca_compartment_is_compartment(oca,compartmenta,sbu,unclassified,no,no) )).

fof(ax86,hypothesis,(
    system_indi_is_counterintelligence(system,ci,owner_secretfile) )).

fof(ax79,hypothesis,(
    system_indi_needs_compartment(system,alice,compartmenta) )).

fof(ax51,hypothesis,(
    system_file_needs_compartments(system,secretfile,cons(compartmentb,cons(compartmenta,nil))) )).

fof(ax65,hypothesis,(
    state_file_has_owner(not_secretfile,owner_not_secretfile) )).

fof(ax45,hypothesis,(
    oca_compartment_has_scg(oca,compartmentb,scg_compartmentb) )).

fof(ax80,hypothesis,(
    sso_indi_has_compartment(sso_compartmentb,alice,compartmentb) )).

fof(ax60,hypothesis,(
    state_file_has_owner(secretfile,owner_secretfile) )).

fof(ax63,hypothesis,(
    system_file_needs_level(system,not_secretfile,unclassified) )).

fof(ax78,hypothesis,(
    system_indi_needs_compartment(system,alice,compartmentb) )).

fof(ax56,hypothesis,(
    sso_file_has_level(sso_compartmenta,secretfile,secret,scg_compartmenta) )).

fof(ax85,hypothesis,(
    owner_indi_has_need_to_know(owner_not_secretfile,babu,not_secretfile) )).

fof(ax75,hypothesis,(
    hr_admin_indi_has_employment(hr_admin,alice) )).

fof(ax61,hypothesis,(
    state_file_is_not_working_paper(not_secretfile) )).

fof(ax69,hypothesis,(
    system_indi_is_hr_admin(system,hr_admin) )).

fof(ax44,hypothesis,(
    system_compartment_has_sso(system,compartmentb,sso_compartmentb) )).

fof(ax76,hypothesis,(
    system_indi_needs_level(system,alice,secret) )).

fof(ax50,hypothesis,(
    state_file_is_not_working_paper(secretfile) )).

fof(ax57,hypothesis,(
    system_file_needs_citizenship(system,secretfile,usa) )).

fof(ax70,hypothesis,(
    system_indi_is_level_admin(system,level_admin) )).

fof(ax49,hypothesis,(
    sso_compartment_has_scg(sso_compartmenta,compartmenta,scg_compartmenta) )).

fof(ax72,hypothesis,(
    polygraph_admin_indi_has_polygraph(polygraph_admin,alice) )).

fof(ax67,hypothesis,(
    system_indi_is_credit_admin(system,credit_admin) )).

fof(ax84,hypothesis,(
    system_indi_has_citizenship(system,babu,india) )).

fof(ax83,hypothesis,(
    owner_indi_has_need_to_know(owner_secretfile,alice,not_secretfile) )).

fof(ax71,hypothesis,(
    system_indi_has_citizenship(system,alice,usa) )).

fof(ax47,hypothesis,(
    system_compartment_has_sso(system,compartmenta,sso_compartmenta) )).

fof(ax73,hypothesis,(
    credit_admin_indi_has_credit(credit_admin,alice) )).

fof(ax46,hypothesis,(
    sso_compartment_has_scg(sso_compartmentb,compartmentb,scg_compartmentb) )).

fof(ax81,hypothesis,(
    sso_indi_has_compartment(sso_compartmenta,alice,compartmenta) )).

fof(ax59,hypothesis,(
    sso_file_has_citizenship(sso_compartmenta,secretfile,usa,scg_compartmenta) )).

fof(ax55,hypothesis,(
    sso_file_has_level(sso_compartmentb,secretfile,secret,scg_compartmentb) )).

fof(ax74,hypothesis,(
    background_admin_indi_has_background(background_admin,alice,topsecret) )).

fof(ax77,hypothesis,(
    level_admin_indi_has_level(level_admin,alice,topsecret) )).

fof(ax53,hypothesis,(
    sso_file_has_compartments(sso_compartmenta,secretfile,cons(compartmentb,cons(compartmenta,nil))) )).

fof(ax54,hypothesis,(
    system_file_needs_level(system,secretfile,secret) )).

fof(alicereadnotsecret,conjecture,(
    admin_indi_may_file(admin,alice,not_secretfile,read) )).

fof(ax52,hypothesis,(
    sso_file_has_compartments(sso_compartmentb,secretfile,cons(compartmentb,cons(compartmenta,nil))) )).

fof(ax62,hypothesis,(
    system_file_needs_compartments(system,not_secretfile,nil) )).

fof(ax82,hypothesis,(
    owner_indi_has_need_to_know(owner_secretfile,alice,secretfile) )).

fof(ax66,hypothesis,(
    system_indi_is_polygraph_admin(system,polygraph_admin) )).

fof(ax58,hypothesis,(
    sso_file_has_citizenship(sso_compartmentb,secretfile,usa,scg_compartmentb) )).

fof(ax42,hypothesis,(
    oca_compartment_is_compartment(oca,compartmentb,confidential,topsecret,yes,yes) )).

