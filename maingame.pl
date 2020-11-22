:- dynamic(player/2).
:- dynamic(encounter/1).
:- dynamic(started/1).
:- dynamic(inventory/2).
:- dynamic(equipped/2).
:- dynamic(job/1).
:- dynamic(playerData/7).
:- dynamic(inStore/1).

tiles(4,4).
tiles(5,4).
tiles(6,4).
tiles(4,5).
tiles(4,6).

player(9,9).

:- dynamic(job/1).


bosNaga(1,1).
store(9, 7).
quest(1, 10).

/* LIST NAMA Item */

displayItemName(none, 'None').
displayItemName(healing_potion, 'Healing Potion').

displayItemName(wooden_sword, 'Wooden Sword (Swordsman)').
displayItemName(stone_sword, 'Stone Sword (Swordsman)').
displayItemName(iron_sword, 'Iron Sword (Swordsman)').
displayItemName(diamond_sword,'Diamond Sword (Swordsman)').

displayItemName(studded_armor, 'Studded Armor (Swordsman)').
displayItemName(iron_armor , 'Iron Armor (Swordsman)').
displayItemName(reinforced_armor, 'Reinforced Armor (Swordsman)').

displayItemName(wooden_bow, 'Wooden Bow (Archer)').
displayItemName(compound_bow, 'Compound Bow (Archer)').
displayItemName(fire_bow, 'Fire Bow (Archer)').

displayItemName(leather_armor, 'Leather Armor (Archer)').
displayItemName(chain_armor, 'Chain Armor (Archer)').
displayItemName(scale_armor, 'Scale Armor (Archer)').

displayItemName(flame_staff, 'Flame Staff (Sorcerer)').
displayItemName(thunderbolt_staff, 'Thunderbolt Staff (Sorcerer)').
displayItemName(void_staff, 'Void Staff (Sorcerer)').

displayItemName(novice_robe, 'Novice Robe (Sorcerer)').
displayItemName(apprentice_robe, 'Apprentice Robe (Sorcerer)').
displayItemName(expert_robe, 'Expert Robe (Sorcerer)').

/*--------------------------------------------------------------------------*/

/* MAIN */
started(no).

start :-
    /*playerData(Level,HP,MaxHP,Att,Def,Exp,Gold)*/
    started(no), !,
    write('Welcome to Genshin Asik. Choose your job'),nl,
    write('1. Swordsman'),nl,
    write('2. Archer'),nl,
    write('3. Sorcerer'),nl,
    read(JobNum),
    ((
    JobNum = 1, 
    asserta(job(swordsman)),
    asserta(playerData(1,100,100,10,6,0,0)),
    write('Anda memilih class Swordsman, good luck boi'), 
    nl);
    (
    JobNum = 2, 
    asserta(job(archer)),
    asserta(playerData(1,80,80,12,4,0,0)),
    write('Anda memilih class Archer, good luck boi'), 
    nl);
    (
    JobNum = 3, 
    asserta(job(sorcerer)), 
    asserta(playerData(1,60,60,15,3,0,0)),
    write('Anda memilih class Sorcerer, good luck boi'), 
    nl
    )),
    asserta(inventory([],0)),
    retract(started(no)),
    asserta(started(yes)),!,

    map.

start :-
    started(yes), !,
    write('Game sudah mulai'), nl.


/*--------------------------------------------------------------------------*/

/* MAP */

printMap(X,Y) :-
    printBorderTB(X),
    Yt is Y-2,
    Xt is X-1,
    printCenterMap(Xt,Yt).

printBorderTB(1) :- 
    write('# '),
    nl.
printBorderTB(X) :-
    X>1,
    write('# '),
    A is X-1,
    printBorderTB(A).

printCenterMap(Xt,Yt) :-
    Yt > 0,
    initPrintCenterX(Xt,Yt),
    Yt1 is Yt-1,
    printCenterMap(Xt,Yt1).

printCenterMap(Xt,0) :-
    X is Xt+1,
    printBorderTB(X).
    

initPrintCenterX(Xt,Yt) :-
    write('# '),
    XMid is Xt-1,
    printCenterX(XMid,Yt).

printCenterX(XMid,Y):-
    XMid > 0,
    tiles(XMid,Y),
    write('# '),
    XtMid is XMid-1,
    printCenterX(XtMid,Y),!.

printCenterX(XMid,Y):-
    XMid > 0,
    bosNaga(XMid,Y),
    write('D '),
    XtMid is XMid-1,
    printCenterX(XtMid,Y),!.

printCenterX(XMid, Y) :-
    XMid > 0,
    store(XMid, Y),
    write('S '),
    XtMid is XMid - 1,
    printCenterX(XtMid, Y), !.

printCenterX(XMid, Y) :-
    XMid > 0,
    quest(XMid, Y),
    write('Q '),
    XtMid is XMid - 1,
    printCenterX(XtMid, Y), !.

printCenterX(XMid,Y):-
    XMid > 0,
    player(XMid,Y),
    write('P '),
    XtMid is XMid-1,
    printCenterX(XtMid,Y),!.

printCenterX(XMid,Y):-
    XMid > 0,
    \+ tiles(XMid,Y),
    \+ bosNaga(XMid,Y),
    \+ store(XMid, Y),
    \+ quest(XMid, Y),
    \+ player(XMid,Y),
    write('- '),
    XtMid is XMid-1,
    printCenterX(XtMid,Y),!.

printCenterX(0,_):-
    printBorderTB(1),!.

map :- 
    started(yes), !,
    printMap(17, 17), !.

map :-
    started(no), !,
    write('Anda belum start game'), nl.

/*--------------------------------------------------------------------------*/

/* WASD */

w :-
    started(yes),
    inStore(yes),!,
    write('Anda sedang di dalam store.'), nl, 
    leave_menu.

w :-
    started(yes),
    encounter(no),
    player(X, Y),
    Y2 is Y + 1,
    X2 is X,
    Y2 =\= 16,
    (\+ tiles(X2, Y2)),
    asserta(player(X2, Y2)),
    retract(player(X, Y)), !,
    %map,
    write('Anda bergerak satu langkah ke Utara'), nl,
    check_lock(X2,Y2), !.


w :-
    started(yes),
    encounter(no),
    write('Anda tertabrak'), nl, !.

w :-
    started(yes),
    encounter(yes),
    write('Anda sedang dalam battle'), nl, !.

w :-
    started(no),
    encounter(no),
    write('Anda belum memulai game'), nl, !.

a :-
    started(yes),
    inStore(yes),!,
    write('Anda sedang di dalam store.'), nl, 
    leave_menu.

a :-
    started(yes),
    encounter(no),
    player(X, Y),
    X2 is X + 1,
    Y2 is Y,
    X2 =\= 16,
    (\+ tiles(X2, Y2)),
    asserta(player(X2, Y2)),
    retract(player(X, Y)), !,
    %map,
    write('Anda bergerak satu langkah ke Barat'), nl,
    check_lock(X2,Y2), !.

a :-
    started(yes),
    encounter(no),
    write('Anda tertabrak'), nl, !.

a :-
    started(yes),
    encounter(yes),
    write('Anda sedang dalam battle'), nl, !.

a :-
    started(no),
    encounter(no),
    write('Anda belum memulai game'), nl, !.

s :-
    started(yes),
    inStore(yes),!,
    write('Anda sedang di dalam store.'), nl,
    leave_menu.

s :-
    started(yes),
    encounter(no),
    player(X, Y),
    Y2 is Y - 1,
    X2 is X,
    Y2 =\= 0,
    (\+ tiles(X2, Y2)),
    asserta(player(X2, Y2)),
    retract(player(X, Y)), !,
    %map,
    write('Anda bergerak satu langkah ke Selatan'), nl,
    check_lock(X2,Y2), !.

s :-
    started(yes),
    encounter(no),
    write('Anda tertabrak'), nl, !.

s :-
    started(yes),
    encounter(yes),
    write('Anda sedang dalam battle'), nl, !.

s :-
    started(no),
    encounter(no),
    write('Anda belum memulai game'), nl, !.

d :-
    started(yes),
    inStore(yes),!,
    write('Anda sedang di dalam store.'), nl, 
    leave_menu.

d :-
    started(yes),
    encounter(no),
    player(X, Y),
    X2 is X - 1,
    Y2 is Y,
    X2 =\= 0,
    (\+ tiles(X2, Y2)),
    asserta(player(X2, Y2)),
    retract(player(X, Y)), !,
    %map,
    write('Anda bergerak satu langkah ke Timur'), nl,
    check_lock(X2,Y2), !.

d :-
    started(yes),
    encounter(no),
    write('Anda tertabrak'), nl, !.

d :-
    started(yes),
    encounter(yes),
    write('Anda sedang dalam battle'), nl, !.

d :-
    started(no),
    encounter(no),
    write('Anda belum memulai game'), nl, !.


/*--------------------------------------------------------------------------*/

/* Status Player */
/* playerData([], [], [], [], []).
playerData([A|V], [B|W], [C,X], [D,Y], [E,Z]) :- */

%playerData(15, 10, 20).
%playerLVL(1, 0).
%playerGold(0).

printJob :-
    job(swordsman),!,
    write('Swordsman').
printJob :-
    job(archer),!,
    write('Archer').
printJob :-
    job(sorcerer),!,
    write('Sorcerer').

status :-
    playerData(Level,HP,MaxHP,Att,Def,Exp,Gold),
    job(Job),
    exp(XP),
    ToNextLVL is XP - Exp,
    write('Your status : '),  nl,
    write('Job     : '),printJob, nl,
    format('Level   : ~w', [Level]), nl,
    format('Health  : ~w/~w', [HP,MaxHP]), nl,
    format('Attack  : ~w', [Att]) , nl,
    format('Defense : ~w', [Def]), nl,
    format('Exp     : ~w', [Exp]), nl,
    format('Gold    : ~w', [Gold]), nl,
    format('Next LVL: ~w Exp', [ToNextLVL]), nl.


/*--------------------------------------------------------------------------*/
/* Enemy Status */
:- dynamic(slime/4).
:- dynamic(wolf/4).
:- dynamic(goblin/4).
% enemy(LVL, ATT, DEF, HP)
slime(1, 15, 0, 27). %kalo gini damagenya gabisa ngedamage player
wolf(1, 16, 4, 25).
goblin(1, 18, 3, 33).
/* Enemy Encounter */
encounter(no).

start_encounter :-
    encounter(no),
    retract(encounter(no)),
    asserta(encounter(yes)).

end_encounter :-
    encounter(yes),
    retract(encounter(yes)),
    asserta(encounter(no)),
    retract(special_counter(_)),
    asserta(special_counter(0)),
    ((retract(battle_slime(_,_,_))); (retract(battle_wolf(_,_,_))); (retract(battle_goblin(_,_,_))); (retract(battle_metalslime(_)))).
    /* tambahin retract special_counter */
    /* CEK */

end_battle :-
    encounter(yes),
    retract(encounter(yes)),
    asserta(encounter(no)),
    retract(special_counter(_)),
    asserta(special_counter(0)).

encounter_chance(X) :-
    between(1, 20, X),
    start_encounter,
    write('Anda bertemu dengan slime'), nl,
    battle_menu,
    slime(_, Attack, Defense, HP),
    playerData(LVL, _, _, _, _, _, _),
    NewAttack is Attack + (LVL * 1),
    NewDefense is Defense + (LVL * 1),
    NewHP is HP + (LVL * 3),
    asserta(battle_slime(NewAttack, NewDefense, NewHP)),
    status_enemy, !.

encounter_chance(X) :-
    between(21, 35, X),
    start_encounter,
    write('Anda bertemu dengan wolf'), nl,
    battle_menu,
    wolf(_, Attack, Defense, HP),
    playerData(LVL, _, _, _, _, _, _),
    NewAttack is Attack + (LVL * 1.75),
    NewDefense is Defense + (LVL * 0.75),
    NewHP is HP + (LVL * 5),
    asserta(battle_wolf(NewAttack, NewDefense, NewHP)),
    status_enemy, !.

encounter_chance(X) :-
    between(41, 50, X),
    start_encounter,
    write('Anda bertemu dengan goblin'), nl,
    battle_menu,
    goblin(_, Attack, Defense, HP),
    playerData(LVL, _, _, _, _, _, _),
    NewAttack is Attack + (LVL * 1.75),
    NewDefense is Defense + (LVL * 1.75),
    NewHP is HP + (LVL * 7),
    asserta(battle_goblin(NewAttack, NewDefense, NewHP)),
    status_enemy, !.

encounter_chance(X) :-
    between(61, 65, X),
    start_encounter,
    write('Anda bertemu dengan metal slime'), nl,
    battle_menu,
    random(75, 121, HP),
    asserta(battle_metalslime(HP)),
    status_enemy, !.
    /*  Lvl : ???
        Att : ???
        Def : ???
        HP  : ??? 
        gives ton of exp (rekursi lvl up)
        add enemy run ???*/

encounter_chance(101) :-
    start_encounter,
    write('Anda bertemu dengan bosNaga'), nl.

check_lock(X, Y) :-
    X >= 13,
    Y >= 13,
    random(-2, 6, Z), %80 persen bertemu dengan slime
    encounter_chance(Z), !.

check_lock(X, Y) :-
    X >= 13,
    Y =< 3,
    random(31, 39, Z),
    encounter_chance(Z), !.

check_lock(X, Y) :-
    X =< 3,
    Y >= 13, 
    random(45, 53, Z),
    encounter_chance(Z), !.

check_lock(13, 6) :-
    random(64, 68, Z), !,
    encounter_chance(Z), !.

/*  ADD Store */

check_lock(X, Y) :-
    \+ store(X, Y),
    \+ quest(X, Y),
    \+ bosNaga(X, Y),
    random(1, 101, Z),
    encounter_chance(Z).

check_lock(X,Y) :-
    store(X,Y), !,
    retract(inStore(no)),
    asserta(inStore(yes)),
    write('Selamat datang di Store!'), nl,
    store_menu.

check_lock(X, Y) :-
    bosNaga(X, Y), !,
    encounter_chance(101).

check_lock(X, Y) :-
    quest(X, Y), !,
    write('Anda berada dalam Guild, ambil quest?').

battle_menu :-
    write('Apa yang Anda akan lakukan?'), nl,
    write('Attack?'), nl,
    write('Use Potion?'), nl,
    write('Run?'), nl, !.
/*
status_enemy :-
    battle_slime(_, _, HP), !,
    format('Level  : ~w', [Lvl]), nl,
    format('Attack : ~w', [Att]), nl,
    format('Defense: ~w', [Def]), nl,
    format('HP     : ~w/~w', [HP, HPCap]), nl, !. */

status_enemy :-
    battle_slime(Att, Def, HP), !,
    playerData(Lvl, _, _, _, _, _, _),
    format('Level  : ~w', [Lvl]), nl,
    format('Attack : ~w', [Att]), nl,
    format('Defense: ~w', [Def]), nl,
    format('HP     : ~w', [HP]), nl, !.

status_enemy :-
    battle_wolf(Att, Def, HP), !,
    playerData(Lvl, _, _, _, _, _, _),
    format('Level  : ~w', [Lvl]), nl,
    format('Attack : ~w', [Att]), nl,
    format('Defense: ~w', [Def]), nl,
    format('HP     : ~w', [HP]), nl, !.

status_enemy :-
    battle_goblin(Att, Def, HP), !,
    playerData(Lvl, _, _, _, _, _, _),
    format('Level  : ~w', [Lvl]), nl,
    format('Attack : ~w', [Att]), nl,
    format('Defense: ~w', [Def]), nl,
    format('HP     : ~w', [HP]), nl, !.

status_enemy :-
    battle_metalslime(HP), !,
    write('Level  : ???'), nl,
    write('Attack : ???'), nl,
    write('Defense: ???'), nl,
    format('HP     : ~w', [HP]), nl, !.

status_enemy :-
    started(no),
    write('Anda belum mulai game'), nl, !.

status_enemy :-
    started(yes),
    encounter(no),
    write('Anda tidak sedang dalam battle'), nl, !.

enemy_zone :-
    quest_counter(X),
    X >= 5, !,
    write('Slime  Zone      : X > 13, Y > 13'), nl,
    write('Wolf   Zone      : X > 13, Y < 5'), nl,
    write('Goblin Zone      : X < 5 , Y > 13'), nl,
    write('Metal Slime Zone : X = 13, Y = 6'), nl.

enemy_zone :-
    quest_counter(X),
    X < 5, !,
    write('Slime  Zone      : X >  13, Y >  13'), nl,
    write('Wolf   Zone      : X >  13, Y <  5'), nl,
    write('Goblin Zone      : X <  5 , Y >  13'), nl,
    write('Metal Slime Zone : X = ???, Y = ???'), nl.

/*--------------------------------------------------------------------------*/
/* TEST*/
:- dynamic(special_counter/1).
%player(10, 10, 40). %Swordsman
special_counter(0).
:- dynamic(battle_goblin/3).
:- dynamic(battle_slime/3).
:- dynamic(battle_wolf/3).
:- dynamic(exp/1).
exp(50). %exp untuk level up
/* Battle Mechanism */
run :-
    encounter(yes),
    random(1, 11, X),
    run_success(X), !.

run :-
    encounter(no),
    write('Anda tidak sedang di dalam battle'), nl.

run_success(X) :-
    X > 4,
    write('Run gagal, turn diberikan ke musuh'), nl,
    ((slime_attack);(wolf_attack);(slime_attack);(metalslime_attack)).

run_success(X) :-
    between(1, 4, X),
    write('Run berhasil'), nl,
    end_encounter.

/* Ganti player() dengan fakta yang baru */
/* Tambah ATTACK dari enemy */
/* Attacking Slime */
/*playerData(Level,HP,MaxHP,Att,Def,Exp,Gold)*/
attack :-
    encounter(yes),
    battle_slime(Attack, Defense, HP), !,
    playerData(A, B, C, AttackP, D, E, F),
    AttDealt is AttackP - Defense,
    NewHP is HP - AttackP + Defense,
    format('You dealt ~w damage to the Slime', [AttDealt]), nl,
    retract(battle_slime(_, _, _)),
    asserta(battle_slime(Attack, Defense, NewHP)),
    check_death_slime,
    slime_attack,
    special_counter(Count),
    0 =\= mod(Count, 3), !,
    special_increment.
    
/* Attacking Wolf */
attack :-
    encounter(yes),
    battle_wolf(Attack, Defense, HP), !,
    playerData(A, B, C, AttackP, D, E, F),
    AttDealt is AttackP - Defense,
    NewHP is HP - AttackP + Defense,
    format('You dealt ~w damage to the Wolf', [AttDealt]), nl,
    retract(battle_wolf(_, _, _)),
    asserta(battle_wolf(Attack, Defense, NewHP)),
    check_death_wolf,
    wolf_attack,
    special_counter(Count),
    0 =\= mod(Count, 3), !,
    special_increment.

/* Attacking Goblin */
attack :-
    encounter(yes),
    battle_goblin(Attack, Defense, HP), !,
    playerData(A, B, C, AttackP, D, E, F),
    AttDealt is AttackP - Defense,
    NewHP is HP - AttackP + Defense,
    format('You dealt ~w damage to the Goblin', [AttDealt]), nl,
    retract(battle_goblin(_, _, _)),
    asserta(battle_goblin(Attack, Defense, NewHP)),
    check_death_goblin,
    goblin_attack,
    special_counter(Count),
    0 =\= mod(Count, 3), !,
    special_increment.

/* Attacking Metal Slime */
attack :-
    encounter(yes),
    battle_metalslime(HP), !,
    NewHP is HP - 1,
    write('You dealt 1 damage to the Metal Slime'), nl,
    retract(battle_metalslime(_)),
    asserta(battle_metalslime(NewHP)),
    check_death_metalslime,
    metalslime_attack,
    special_counter(Count),
    0 =\= mod(Count, 3), !,
    special_increment.


/* CEK */
attack :-
    started(yes),
    encounter(no),
    write('Anda tidak sedang di dalam battle'), nl, !.

attack :-
    started(no),
    encounter(no),
    write('Anda belum memulai game'), nl, !.

check_death_slime :-
    battle_slime(_, _, X),
    X > 0, !.

check_death_slime :-
    battle_slime(_, _, X), !,
    X =< 0,
    retract(battle_slime(_, _, _)),
    dec_slime, %%%%% TEST
    /* Insert Quest Counter Here */
    end_battle, %Special Attack di reset saat end_encounter
    write('Slime defeated, great job!'), nl,
    add_gold(10),
    add_exp(15), !, fail. %latest updating

check_death_wolf :-
    battle_wolf(_, _, X),
    X > 0, !.

check_death_wolf :-
    battle_wolf(_, _, X), !,
    X =< 0, !,
    retract(battle_wolf(_, _, _)),
    dec_wolf, %%%%% TEST
    /* Insert Quest Counter Here */
    end_battle,
    write('Wolf defeated, great job!'), nl,
    add_gold(50),
    add_exp(20), !, fail.

check_death_goblin :-
    battle_goblin(_, _, X),
    X > 0, !.

check_death_goblin :-
    battle_goblin(_, _, X), !,
    X =< 0, !,
    retract(battle_goblin(_, _, _)),
    dec_goblin, %%%%% TEST
    /* Insert Quest Counter Here */
    end_battle,
    write('Goblin defeated, great job!'), nl,
    add_gold(100),
    add_exp(30), !, fail.

check_death_metalslime :-
    battle_metalslime(HP),
    HP > 0, !.

check_death_metalslime :-
    battle_metalslime(HP), !,
    HP =< 0, !,
    retract(battle_metalslime(_)),
    /* No Quest Counter Here */
    end_battle,
    write('Metal Slime defeated, great job!'), nl,
    playerData(LVL, _, _, _, _, _, _),
    EXPGiven is LVL * 150,
    add_gold(100),
    add_exp(EXPGiven).

add_gold(X) :-
    format('You got ~w gold coins', [X]), nl,
    retract(playerData(LVL, HP, MAXHP, Att, Def, Exp, Gold)),
    NewGold is Gold + X,
    asserta(playerData(LVL, HP, MAXHP, Att, Def, Exp, NewGold)).

/* Leveling System */

add_exp(X) :-
    format('You got ~w Experience Points', [X]), nl,
    playerData(LVL, HP, MaxHP, Att, Def, EXP, Gold),
    exp(MaxEXP),
    NewEXP is EXP + X,
    levelup_check(NewEXP, MaxEXP), !,
    %Jika levelup_check berhasil akan levelup
    level_up(NewEXP, MaxEXP).

levelup_check(Exp, ExpCap) :-
    Exp >= ExpCap, !.

levelup_check(Exp, ExpCap) :-
    Exp < ExpCap,
    playerData(LVL, HP, MaxHP, Att, Def, EXP, Gold),
    retract(playerData(_, _, _, _, _, _, _)),
    asserta(playerData(LVL, HP, MaxHP, Att, Def, Exp, Gold)), fail.

level_up(EXP, EXPCap) :- %Basis Level Up
    EXP < EXPCap, !.

level_up(EXP, EXPCap) :- %input EXP sudah ditambahkan dengan Exp Player awal
    playerData(LVL, HP, MAXHP, Att, Def, Exp, Gold),
    NewEXP is EXP - EXPCap,
    NewEXP >= 0, !,
    NewEXPCap is EXPCap + 10,
    retract(playerData(_, _, _, _, _, _, _)),
    retract(exp(_)),
    NewLVL is LVL + 1,
    NewHP is HP + 10,
    NewMaxHP is MAXHP + 10,
    NewAtt is Att + 2,
    NewDef is Def + 1,
    asserta(playerData(NewLVL, NewHP, NewMaxHP, NewAtt, NewDef, NewEXP, Gold)),
    asserta(exp(NewEXPCap)),
    level_up(NewEXP, NewEXPCap), !. %Rekursi Level Up


/* Slime Attack */
/*playerData(Level,HP,MaxHP,Att,Def,Exp,Gold)*/

slime_attack :-
    battle_slime(Att, _, _),
    playerData(LVL, HP, MHP, AttP, Def, Exp, Gold),
    AttDealt is Att - Def,
    AttDealt > 0, !,
    format('You take ~w damage from the Slime', [AttDealt]), nl,
    NewHP is HP - AttDealt,
    retract(playerData(_, _, _, _, _, _, _)),
    asserta(playerData(LVL, NewHP, MHP, AttP, Def, Exp, Gold)),
    check_player_death.

slime_attack :-
    battle_slime(Att, _, _),
    playerData(LVL, HP, MHP, AttP, Def, Exp, Gold),
    AttDealt is Att - Def,
    AttDealt =< 0, !,
    write('You take no damage from the Slime'), nl.

/* Wolf Attack */

wolf_attack :-
    battle_wolf(Att, _, _),
    playerData(LVL, HP, MHP, AttP, Def, Exp, Gold),
    AttDealt is Att - Def,
    AttDealt > 0, !,
    format('You take ~w damage from the Wolf', [AttDealt]), nl,
    NewHP is HP - AttDealt,
    retract(playerData(_, _, _, _, _, _, _)),
    asserta(playerData(LVL, NewHP, MHP, AttP, Def, Exp, Gold)),
    check_player_death.

wolf_attack :-
    battle_wolf(Att, _, _),
    playerData(LVL, HP, MHP, AttP, Def, Exp, Gold),
    AttDealt is Att - Def,
    AttDealt =< 0, !,
    write('You take no damage from the Wolf'), nl.

/* Goblin Attack */

goblin_attack :-
    battle_goblin(Att, _, _),
    playerData(LVL, HP, MHP, AttP, Def, Exp, Gold),
    AttDealt is Att - Def,
    AttDealt > 0, !,
    format('You take ~w damage from the Goblin', [AttDealt]), nl,
    NewHP is HP - AttDealt,
    retract(playerData(_, _, _, _, _, _, _)),
    asserta(playerData(LVL, NewHP, MHP, AttP, Def, Exp, Gold)),
    check_player_death.

goblin_attack :-
    battle_goblin(Att, _, _),
    playerData(LVL, HP, MHP, AttP, Def, Exp, Gold),
    AttDealt is Att - Def,
    AttDealt =< 0, !,
    write('You take no damage from the Goblin'), nl.

/* Metal Slime Attack */

metalslime_attack :-
    battle_metalslime(_),
    random(1, 101, Randomize),
    Randomize > 60, !,
    random(1, 4, AttDealt),
    playerData(LVL, HP, MHP, AttP, Def, Exp, Gold),
    AttDealt > 0, !,
    format('You take ~w damage from the Metal Slime', [AttDealt]), nl,
    NewHP is HP - AttDealt,
    retract(playerData(_, _, _, _, _, _, _)),
    asserta(playerData(LVL, NewHP, MHP, AttP, Def, Exp, Gold)),
    check_player_death.

metalslime_attack :-
    battle_metalslime(_),
    random(1, 101, Randomize),
    Randomize > 50, !,
    write('You take no damage from the Metal Slime'), nl.

metalslime_attack :-
    battle_metalslime(_),
    write('Metal Slime has ran away, good luck next time'), nl,
    end_encounter.

/*playerData(Level,HP,MaxHP,Att,Def,Exp,Gold)*/
check_player_death :-
    playerData(LVL, HP, MaxHP, Att, Def, Exp, Gold),
    HP > 0, !.

check_player_death :-
    playerData(LVL, HP, MaxHP, Att, Def, Exp, Gold),
    HP =< 0, !,
    end_battle,
    write('You have been defeated, goodbye friend'), nl,
    retract(started(_)),
    retract(playerData(_, _, _, _, _, _, _)),
    retract(job(_)),
    asserta(started(no)),
    retract(in_quest(_)),
    asserta(in_quest(no)),
    retract(encounter(_)),
    asserta(encounter(no)),
    retract(quest_counter(_)),
    asserta(quest_counter(0)),
    (
        (retract(battle_slime(_,_,_))); 
        (retract(battle_wolf(_,_,_))); 
        (retract(battle_goblin(_,_,_))); 
        (retract(battle_metalslime(_))); 
        (retract(quest(_, _, _, _, _)))
        %%%%% ADD TIAP FAKTA YANG KALO MATI DI RETRACT
    ).

/* Add Enemy Attacking to Special Attack */
%playerData(_, _, _, AttackP, _, _, _),
/* Special Attacking Slime */
specialAttack :-
    encounter(yes),
    playerData(A, B, C, AttP, D, E, F),
    battle_slime(Att, Def, HP), !,
    special_counter(Count),
    check_special(Count), !,
    retract(battle_slime(_, _, _)),
    NewAttP is AttP * 3,
    AttDealt is NewAttP - Def,
    NewHP is HP - NewAttP + Def, 
    asserta(battle_slime(Att, Def, NewHP)),
    format('You dealt ~w damage to the Slime', [AttDealt]), nl,
    special_increment,
    check_death_slime,
    slime_attack.

/* Special Attacking Wolf */
specialAttack :-
    encounter(yes),
    playerData(A, B, C, AttP, D, E, F),
    battle_wolf(Att, Def, HP), !,
    special_counter(Count),
    check_special(Count), !,
    retract(battle_wolf(_, _, _)),
    NewAttP is AttP * 3,
    AttDealt is NewAttP - Def,
    NewHP is HP - NewAttP + Def,
    format('You dealt ~w damage to the Wolf', [AttDealt]), nl,
    asserta(battle_wolf(Att, Def, NewHP)),
    special_increment,
    check_death_wolf,
    wolf_attack.

/* Special Attacking Goblin */
specialAttack :-
    encounter(yes),
    playerData(A, B, C, AttP, D, E, F),
    battle_goblin(Att, Def, HP), !,
    special_counter(Count),
    check_special(Count), !,
    retract(battle_goblin(_, _, _)),
    NewAttP is AttP * 3,
    AttDealt is NewAttP - Def,
    NewHP is HP - NewAttP + Def,
    format('You dealt ~w damage to the Goblin', [AttDealt]), nl,
    asserta(battle_goblin(Att, Def, NewHP)),
    special_increment,
    check_death_goblin,
    goblin_attack.

/* Special Attacking Metal Slime */
specialAttack :-
    encounter(yes),
    playerData(A, B, C, AttP, D, E, F),
    battle_metalslime(HP), !,
    special_counter(Count),
    check_special(Count), !,
    retract(battle_metalslime(_)),
    NewAttP is AttP * 3,
    NewHP is HP - NewAttP,
    format('You dealt ~w damage to the Metal Slime', [NewAttP]), nl,
    asserta(battle_metalslime(NewHP)),
    special_increment,
    check_death_metalslime,
    metalslime_attack.

specialAttack :-
    encounter(yes),
    player(AttP, _, _),
    special_counter(Count),
    X is mod(Count, 3),
    0 =\= mod(Count, 3), !,
    write('You have to wait ~w more turns to use special attack', [X]), nl.

specialAttack :-
    started(yes),
    encounter(no),
    write('Anda tidak sedang di dalam battle'), nl, !.

specialAttack :-
    started(no),
    encounter(no),
    write('Anda belum memulai game'), nl, !.

check_special(X) :-
    0 =:= mod(X, 3), !.

check_special(X) :-
    Y is mod(X, 3),
    Z is 3 - Y,
    format('You have to wait ~w turns to use special attack', [Z]), !, fail.
    % cut and fail %

special_increment :-
    special_counter(Count),
    retract(special_counter(_)),
    NewCount is Count + 1,
    asserta(special_counter(NewCount)).
    
/* Heal */
add_health(Amount) :-
    playerData(LVL, HP, MAXHP, Att, Def, EXP, Gold),
    NewHP is HP + Amount,
    NewHP < MAXHP, !,
    retract(playerData(_, _, _, _, _, _, _)),
    asserta(playerData(LVL, NewHP, MAXHP, Att, Def, EXP, Gold)),
    format('You healed for ~w Health Points', [Amount]), nl.

add_health(Amount) :-
    playerData(LVL, HP, MAXHP, Att, Def, EXP, Gold),
    NewHP is HP + Amount,
    NewHP >= MAXHP, !,
    HPHealed is MAXHP - HP,
    retract(playerData(_, _, _, _, _, _, _)),
    asserta(playerData(LVL, MAXHP, MAXHP, Att, Def, EXP, Gold)),
    format('You healed for ~w Health Points', [HPHealed]), nl.

/*--------------------------------------------------------------------------*/

/* Store */
shop :-
	write('What You Buying?:'),nl,
	write('1. Gacha'), nl,
	write('2. Potion'), nl,
	read(X),
	buy(X),
	!.

buy(1) :-
	playerData(LVL, HP, MAXHP, Att, Def, EXP, Gold),
	Gold > 1000,
	random(1,1000,Rando),
	gacha(Rando).

buy(2) :-
	playerData(LVL, HP, MAXHP, Att, Def, EXP, Gold),
	Gold > 1000,
	EndGold is X - 1000,
	retract(playerData(_, _, _, _, _, _, _)),
	asserta(playerData(LVL, HP, MAXHP, Att, Def, EXP, EndGold)),
	insertOne(healing_potion).
    write('You bought healing potion'),nl.

gacha(X) :-
	X = 1,
	insertOne(diamond_sword),
    write('You got Diamond Sword').
gacha(X) :-
	X > 2, X =< 999,
	insertPlenty(2,wooden_sword),
    write('You got Wooden Sword x2').


/* NON EQUIPMENT PRICE LIST */
/* Format : (ItemName},BuyPrice,SellPrice)*/

nonEqPrice(healing_potion,80,20).

/* GACHA PRICE */
gachaPrice(500).

inStore(no).

add_gold_sell(X) :-
    retract(playerData(LVL, HP, MAXHP, Att, Def, Exp, Gold)),
    NewGold is Gold + X,
    asserta(playerData(LVL, HP, MAXHP, Att, Def, Exp, NewGold)).

store_menu :-
    write('Apa yang anda ingin lakukan?'),nl,
    write('1. Beli barang'),nl,
    write('2. Jual barang'),nl,
    write('3. Pergi dari toko'),nl,
    write('4. Intip inventory'),nl,
    write('5. Intip dompet'),nl,
    read(Pilihan),
    nl,
    ((
        Pilihan == 1,!,
        buy_menu
    );(
        Pilihan == 2,!,
        sell_menu
    );(
        Pilihan == 3,!,
        leave_menu
    );(
        Pilihan == 4,!,
        showInv,nl,
        store_menu
    );(
        Pilihan == 5,!,
        playerData(_,_,_,_,_,_,Gold),
        format('Jumlah Gold Anda : ~w',[Gold]),nl,nl,
        store_menu
    )).

buy_menu :-
    playerData(Lvl,HP,MaxHP,Att,Def,Exp,Gold),
    inventory(_,Capacity),
    maxCapacity(MaxCap),
    write('Apa yang anda ingin beli? (NOTE: Pastikan anda memiliki space inventory yang cukup)'),nl,
    format('Jumlah Item Inventory : (~w/~w)',[Capacity,MaxCap]),nl,
    write('+--------------------------------+'),nl,
    write('| List Item          | Harga     |'),nl,
    write('+-------------------+------------+'),nl,
    write('| 1. Gacha           | 1000 gold |'),nl,
    write('| 2. Healing Potions |   80 gold |'),nl,
    write('+--------------------+-----------+'),nl,
    write('| 3. Tidak jadi beli |    :(     |'),nl,
    write('+--------------------------------+'),nl,
    read(Pilihan),nl,
    ((
        Pilihan == 1,!,
        gachaPrice(X),
        AfterGold is Gold - X,
        ((
            AfterGold < 0,!,
            write('Gold anda tidak cukup.'),nl
        );(
            retract(playerData(_,_,_,_,_,_,_)),
            asserta(playerData(Lvl,HP,MaxHP,Att,Def,Exp,AfterGold)),
            gacha
        ))
        
    );(
        Pilihan == 2,!,
        nonEqPrice(healing_potion,BuyPrice,_),
        write('Berapa banyak? '), read(Amount),
        AfterGold is Gold - Amount * BuyPrice,
        ((
            AfterGold < 0,!,
            write('Gold anda tidak cukup.'),nl
        );(
            insertPlenty(Amount,healing_potion),
            retract(playerData(_,_,_,_,_,_,_)),
            asserta(playerData(Lvl,HP,MaxHP,Att,Def,Exp,AfterGold)),
            write('Transaksi sukses. Terima kasih.'),nl
        ))
    );(
        Pilihan == 3
    )),
    store_menu.

gacha :-
    random(1,113,X),
    ((
        X =< 10,!,
        Item = iron_sword
    );(
        X =< 20,!,
        Item = compound_bow
    );(
        X =< 30,!,
        Item = thunderbolt_staff
    );(
        X =< 40,!,
        Item = studded_armor
    );( 
        X =< 50,!,
        Item = leather_armor      
    );(
        X =< 60,!,
        Item = novice_robe
    );(
        X =< 78,!,
        Item = iron_armor
    );(
        X =< 86,!,
        Item = chain_armor
    );(
        X =< 94,!,
        Item = apprentice_robe
    );(
        X =< 105,!,
        Item = diamond_sword
    );(
        X =< 105,!,
        Item = fire_bow 
    );(
        X =< 110,!,
        Item = void_staff
    );(
        X =< 112,!,
        Item = reinforced_armor
    )),
    displayItemName(Item,DisplayItem),
    format('Selamat! Anda mendapatkan ~w!',[DisplayItem]),nl,
    insertOne(Item).


sell_menu :-
    write('Apa yang anda ingin jual?'),nl,
    write('Format : NamaItem diapit petik 1, lengkap dengan kelas jika equipment.'),nl,
    showInv,
    read(ItemDisName),nl,
    ((
        \+ displayItemName(ItemName,ItemDisName),!,
        write('Anda tidak memiliki item dengan nama itu.'),nl
    );(
        displayItemName(ItemName,ItemDisName),
        \+ search(ItemName),!,
        write('Anda tidak memiliki item dengan nama itu.'),nl
    );( 
        displayItemName(ItemName,ItemDisName),
        playerData(_,_,_,_,_,_,Gold),
        searchItemAmount(ItemName,Amount),
        ((
            equipment(_,_,ItemName,_,Val),!,
            format('Saya akan membeli satu ~w dengan harga ~w.',[ItemDisName, Val]),nl
        );(
            nonEqPrice(ItemName,_,Val),
            format('Saya akan membeli satu ~w dengan harga ~w.',[ItemDisName, Val]),nl
        )),
        format('Anda memiliki ~w ~w. Berapa yang anda ingin jual? ', [Amount,ItemDisName]),
        read(SellAmount),
        ((
            SellAmount > Amount,!,
            write('Jumlah melebihi apa yang anda miliki')
        );(
            equipment(_,_,ItemName,_,Val),!,
            deletePlenty(SellAmount,ItemName),
            TotalGain is Val * SellAmount,
            add_gold_sell(TotalGain),
            format('Terima kasih! Ini ~w gold untuk Anda.',[TotalGain])
        );(
            nonEqPrice(ItemName,_,Val),
            deletePlenty(SellAmount,ItemName),
            TotalGain is Val * SellAmount,
            add_gold_sell(TotalGain),
            format('Terima kasih! Ini ~w gold untuk Anda.', [TotalGain])
        ))
    )),
    nl,
    store_menu.

leave_menu :-
    write('Apakah anda yakin ingin pergi dari toko?'),nl,
    write('1. Ya'),nl,
    write('2. Tidak'),nl,
    read(Pilihan),
    ((
    Pilihan == 1,!,
    retract(inStore(yes)),
    asserta(inStore(no)),
    write('Terima kasih atas kunjungannya. Sampai jumpa!')
    );(
    Pilihan == 2,!,
    store_menu
    )).



/*--------------------------------------------------------------------------*/

/* Quest */
:- dynamic(quest/5).
:- dynamic(in_quest/1).
:- dynamic(quest_counter/1).

quest_counter(0).
in_quest(no).

quest_reward_check :-
    in_quest(yes),
    quest(0, 0, 0, Gold, Exp), !,
    write('Quest Completed!'), nl,
    add_gold(Gold),
    add_exp(Exp),
    retract(in_quest(_)),
    asserta(in_quest(no)),
    retract(quest(_, _, _, _, _)),
    quest_counter(Count),
    NewCount is Count + 1,
    retract(quest_counter(_)),
    asserta(quest_counter(NewCount)).

quest_reward_check :-
    in_quest(yes).

quest_menu :- %%%%% ADD IN QUEST LOCATION?
    in_quest(no), !,
    random(1, 5, Slime1),
    random(1, 4, Wolf1),
    random(1, 3, Goblin1),
    random(1, 5, Slime2),
    random(1, 4, Wolf2),
    random(1, 3, Goblin2),
    random(1, 5, Slime3),
    random(1, 4, Wolf3),
    random(1, 3, Goblin3),
    random(100, 301, Gold1),
    random(100, 301, Gold2),
    random(100, 301, Gold3),
    random(100, 150, Exp1),
    random(100, 150, Exp2),
    random(100, 150, Exp3),
    format('1. Quest 1     : ~w Slimes, ~w Wolves, ~w Goblins', [Slime1, Wolf1, Goblin1]), nl,
    format('   Gold Reward : ~w', [Gold1]), nl,
    format('   Exp Reward  : ~w', [Exp1]), nl,
    format('2. Quest 2     : ~w Slimes, ~w Wolves, ~w Goblins', [Slime2, Wolf2, Goblin2]), nl,
    format('   Gold Reward : ~w', [Gold2]), nl,
    format('   Exp Reward  : ~w', [Exp2]), nl,
    format('3. Quest 3     : ~w Slimes, ~w Wolves, ~w Goblins', [Slime3, Wolf3, Goblin3]), nl,
    format('   Gold Reward : ~w', [Gold3]), nl,
    format('   Exp Reward  : ~w', [Exp3]), nl,
    write('4. Leave.'), nl,
    read(Choice),
    (   
        (
            Choice = 1, 
            asserta(quest(Slime1, Wolf1, Goblin1, Gold1, Exp1)),
            retract(in_quest(no)),
            asserta(in_quest(yes)),
            write('Quest 1 Accepted'), !
        );
        (
            Choice = 2,
            asserta(quest(Slime2, Wolf2, Goblin2, Gold2, Exp2)),
            retract(in_quest(no)),
            asserta(in_quest(yes)),
            write('Quest 2 Accepted'), !
        );
        (
            Choice = 3,
            asserta(quest(Slime3, Wolf3, Goblin3, Gold3, Exp3)),
            retract(in_quest(no)),
            asserta(in_quest(yes)),
            write('Quest 3 Accepted'), !
        )
    ).
%%%%%%%%%%
quest_menu :-
    in_quest(yes), !,
    write('You must finish your current quest first before taking another one'), nl.

quest_status :-
    in_quest(yes), !,
    quest(Slime, Wolf, Goblin, Gold, Exp),
    format('Slimes remaining    : ~w', [Slime]), nl,
    format('Wolves remaining    : ~w', [Wolf]), nl,
    format('Goblins remaining   : ~w', [Goblin]), nl,
    format('Gold reward         : ~w', [Gold]), nl,
    format('Exp reward          : ~w', [Exp]), nl, !.

quest_status :-
    in_quest(no), !,
    write('You\'re currently not taking any quest'), nl,
    write('Go to the Quest Centre to take a quest'), nl.

dec_slime :-
    in_quest(yes),
    quest(Slime, Wolf, Goblin, Gold, Exp),
    Slime > 0, !,
    retract(quest(_, _, _, _, _)),
    NewSlime is Slime - 1,
    asserta(quest(NewSlime, Wolf, Goblin, Gold, Exp)),
    quest_reward_check, !.

dec_slime :-
    in_quest(yes), !.

dec_slime :-
    in_quest(no).

dec_wolf :-
    in_quest(yes),
    quest(Slime, Wolf, Goblin, Gold, Exp),
    Wolf > 0, !,
    retract(quest(_, _, _, _, _)),
    NewWolf is Wolf - 1,
    asserta(quest(Slime, NewWolf, Goblin, Gold, Exp)),
    quest_reward_check, !.

dec_wolf :-
    in_quest(yes), !.

dec_wolf :-
    in_quest(no).

dec_goblin :-
    in_quest(yes),
    quest(Slime, Wolf, Goblin, Gold, Exp),
    Goblin > 0, !,
    retract(quest(_, _, _, _, _)),
    NewGoblin is Goblin - 1,
    asserta(quest(Slime, Wolf, NewGoblin, Gold, Exp)),
    quest_reward_check, !.

dec_goblin :-
    in_quest(yes), !.

dec_goblin :-
    in_quest(no).

save :-
    open('save.txt', write, SAVE),
    set_output(SAVE),
    listing,
    close(S),
    write('Game Saved'), nl.

/* Mungkin Save file nya ga begini, harus dibagi jadi dua file kalo ga nanti pas 
load ada fakta yang double */

load :-
    ['save.txt'],
    write('Game Loaded'), nl.


/*--------------------------------------------------------------------------*/

/* Inventory */

maxCapacity(15).

/* Fungsi dasar stack */

front(Queue,Result) :-
    [H|_] = Queue,
    Result = H.
 
pop(Queue,Result) :-
    [_|T] = Queue,
    Result = T.

push(Element,Queue,Result) :-
    Result = [Element|Queue].

back(Queue,Result) :- 
    [H|T] = Queue, 
    T == [],
    Result = H.
back(Queue,Result) :-
    [_|T] = Queue,
    T \== [],
    pop(Queue,A),
    back(A,Result).

/*Insert into inventory*/

/*insertPlenty untuk memasukkan item berjumlah >= 1 ke inventori dengan cara melakukan insertOne sebanyak jumlah item */
insertPlenty(1,Item) :-
    insertOne(Item),!.
insertPlenty(JumlahItem,Item) :-
    JumlahItem>1,
    insertOne(Item),
    Decr is JumlahItem-1,
    insertPlenty(Decr,Item).

/*insertOne untuk memasukkan 1 item ke inventory*/
insertOne(Item) :-
    inventory(Arr,Capacity),
    maxCapacity(MaxCap),
    Capacity < MaxCap,!,
    ArrInvChecking = Arr,
    insRekursif(Item,ArrInvChecking,[],ResultAkhir),
    TotalItemInventory is Capacity + 1,
    retract(inventory(Arr,Capacity)),
    asserta(inventory(ResultAkhir,TotalItemInventory)).

/* insRekursif, fungsi bantu insertOne untuk memasukkan item ke Inventory; 
    Cara kerja:
    1.      Ambil list Front dari ArrInvChecking, {format Front : [JumlahItem, NamaItem]}

    2.      Keluarkan Front dari ArrInvChecking 

    3.1.    Apabila elemen terakhir dari Front {NamaItem}, sama dengan nama item yang mau dimasukkan ke inventory, JumlahItem di Front 
            diincrement, ArrayPindahan direverse (agar urut), kemudian Front di push ke ArrayPindahan lalu ArrayPindahan di-append atau 
            konkat dengan ArrInvChecking kemudian hasilnya dimasukkan ke ResultAkhir
            
            ResultAkhir kemudian menggantikan array inventory. 
            insRekursif berakhir. 

            Jika tidak, Push Front ke ArrayPindahan hingga ditemukan nama item di list dalam array inventory yang sama dengan nama item
            yang mau dimasukkan atau sampai ArrayInvChecking kosong.

    3,2     Apabila tidak ada item di inventory yang memiliki nama yang sama dengan item yang mau dimasukkan, Item di masukkan
            ke sebuah list bertipe elemen array inventory {[JumlahItem, NamaItem]}, lalu list tersebut di push ke ArrayPindahan yang telah 
            direverse terlebih dahulu (ArrayPindahan = array inventory). Data ArrayPindahan kemudian dimasukkan ke ResultAkhir 

    4.      ResultAkhir kemudian menggantikan Arr pada fakta inventory(Arr,Capacity). 
            insRekursif berakhir. 

    # Note  : Capacity diupdate setelah insRekursif berakhir.

    Format: (Item,ArrInvChecking,ArrayPindahan,ResultAkhir) 
        Item            :   Nama Item
        ArrInvChecking  :   Data array dari inventory
        ArrayPindahan   :   Array yang menampung popped element dari ArrInvChecking
        ResultAkhir     :   Array hasil insRekursif
*/
    
rekInsDelSearch(ArrInvChecking,Front,PoppedArr,Nama1ItemInv) :-
    ArrInvChecking \== [],!,
    front(ArrInvChecking,Front),
    pop(ArrInvChecking,PoppedArr),
    back(Front,Nama1ItemInv).

%Basis
insRekursif(Item,[],ArrayPindahan,ResultAkhir) :-
    reverse(ArrayPindahan,RevArrayPindahan),
    push([1,Item],RevArrayPindahan,ResultAkhir),!.

%Rekurens
insRekursif(Item,ArrInvChecking,ArrayPindahan,ResultAkhir) :-
    rekInsDelSearch(ArrInvChecking,Front,PoppedArr,Nama1ItemInv),
    Item \== Nama1ItemInv,!,
    push(Front,ArrayPindahan,ArrayPindahanT),
    insRekursif(Item,PoppedArr,ArrayPindahanT,ResultAkhir).

insRekursif(Item,ArrInvChecking,ArrayPindahan,ResultAkhir) :-
    rekInsDelSearch(ArrInvChecking,Front,PoppedArr,Nama1ItemInv),
    Item == Nama1ItemInv,!,
    front(Front,Jumlah1ItemInv),
    PlusOneItem is Jumlah1ItemInv + 1,
    PlusOneArr1Item = [PlusOneItem,Nama1ItemInv],
    reverse(ArrayPindahan,RevArrayPindahan),
    push(PlusOneArr1Item,RevArrayPindahan,Result),
    append(Result,PoppedArr,ResultAkhir).


/*Delete item from inventory*/

/*deletePlenty untuk memasukkan item berjumlah >= 1 ke inventori dengan cara melakukan deleteOne sebanyak jumlah item */
deletePlenty(1,Item) :-
    deleteOne(Item),!.
deletePlenty(JumlahItem,Item) :-
    JumlahItem > 1,!,
    deleteOne(Item),
    Decr is JumlahItem-1,
    deletePlenty(Decr,Item).

/*deleteOne untuk memasukkan 1 item ke inventory*/
deleteOne(Item) :-
    inventory(Arr,Capacity),
    Capacity > 0,!,
    ArrInvChecking = Arr,
    delRekursif(Item,ArrInvChecking,[],_).
    
%Basis
delRekursif(_,[],ArrayPindahan,ResultAkhir) :-
    reverse(ArrayPindahan,ResultAkhir).

%Rekurens
delRekursif(Item,ArrInvChecking,ArrayPindahan,ResultAkhir) :-
    rekInsDelSearch(ArrInvChecking,Front,PoppedArr,Nama1ItemInv),

    Item \== Nama1ItemInv,!,

    push(Front,ArrayPindahan,ArrayPindahanT),
    delRekursif(Item,PoppedArr,ArrayPindahanT,ResultAkhir).

delRekursif(Item,ArrInvChecking,ArrayPindahan,ResultAkhir) :-
    rekInsDelSearch(ArrInvChecking,Front,PoppedArr,Nama1ItemInv),

    Item == Nama1ItemInv,!,

    front(Front,Jumlah1ItemInv),
    MinusOneItem is Jumlah1ItemInv-1,
    ((
    MinusOneItem > 0,!,
    MinusOneArr1Item = [MinusOneItem,Nama1ItemInv],
    reverse(ArrayPindahan,RevArrayPindahan),
    push(MinusOneArr1Item,RevArrayPindahan,Result),
    append(Result,PoppedArr,ResultAkhir));
    (
    MinusOneItem =< 0,!,
    reverse(ArrayPindahan,RevArrayPindahan),
    append(RevArrayPindahan,PoppedArr,ResultAkhir)
    )),
    inventory(Arr,Capacity),
    TotalItemInventory is Capacity - 1,
    retract(inventory(Arr,Capacity)),
    asserta(inventory(ResultAkhir,TotalItemInventory)).

/* SHOW INVENTORY */

showInv :-
    inventory(_,Capacity),
    maxCapacity(MaxCap),
    format('Your Inventory (~w/~w):', [Capacity,MaxCap]),nl,
    inventory(Arr,_),
    ArrInvChecking = Arr,
    showRekursif(ArrInvChecking).

showRekursif([]):-
    1 == 1.

showRekursif(ArrInvChecking) :-
    front(ArrInvChecking,Head),
    front(Head,JumlahItem),
    back(Head,NamaItem),
    displayItemName(NamaItem,DisName),
    format('~w ~w', [JumlahItem,DisName]),nl,
    pop(ArrInvChecking,PoppedArr),
    showRekursif(PoppedArr).


/* SEARCH */

search(Item) :-
    inventory(Arr,_),
    ArrInvChecking = Arr,
    searchRekursif(Item,ArrInvChecking,[]).

searchRekursif(_,[],_) :-
    1 == 0.

searchRekursif(Item,ArrInvChecking,ArrayPindahan) :- 
    rekInsDelSearch(ArrInvChecking,Front,PoppedArr,Nama1ItemInv),
    Item \== Nama1ItemInv,!,
    push(Front,ArrayPindahan,ArrayPindahanT),
    searchRekursif(Item,PoppedArr,ArrayPindahanT).

searchRekursif(Item,ArrInvChecking,_) :-
    rekInsDelSearch(ArrInvChecking,_,_,Nama1ItemInv),
    Item == Nama1ItemInv,!,
    1 == 1.


searchItemAmount(Item,Amount) :-
    inventory(Arr,_),
    ArrInvChecking = Arr,
    searchAmRekursif(Item,ArrInvChecking,[],Amount).

searchAmRekursif(_,[],_,Amount) :-
    Amount is -1.

searchAmRekursif(Item,ArrInvChecking,ArrayPindahan,Amount) :- 
    rekInsDelSearch(ArrInvChecking,Front,PoppedArr,Nama1ItemInv),
    Item \== Nama1ItemInv,!,
    push(Front,ArrayPindahan,ArrayPindahanT),
    searchAmRekursif(Item,PoppedArr,ArrayPindahanT,Amount).

searchAmRekursif(Item,ArrInvChecking,_,Amount) :-
    rekInsDelSearch(ArrInvChecking,Front,PoppedArr,Nama1ItemInv),
    Item == Nama1ItemInv,!,
    front(Front,Amount).
    


/*--------------------------------------------------------------------------*/

/* EQUIPMENT */

/* EQUIPMENT LIST */
equipment(weapon, swordsman, wooden_sword, 3, 5).
equipment(weapon, swordsman, stone_sword,  6, 8).
equipment(weapon, swordsman, iron_sword, 9, 12).
equipment(weapon, swordsman, diamond_sword, 12, 20).

equipment(armor, swordsman, studded_armor, 4, 6).
equipment(armor, swordsman, iron_armor, 7, 10).
equipment(armor, swordsman, reinforced_armor, 12, 16).

equipment(weapon, archer, wooden_bow, 5, 5).
equipment(weapon, archer, compound_bow, 10, 8).
equipment(weapon, archer, fire_bow, 15, 12).

equipment(armor, archer, leather_armor, 3, 5).
equipment(armor, archer, chain_armor, 5, 8).
equipment(armor, archer, scale_armor, 9, 12).

equipment(weapon, sorcerer, flame_staff, 8, 5).
equipment(weapon, sorcerer, thunderbolt_staff, 12, 8).
equipment(weapon, sorcerer, void_staff, 18, 12).

equipment(armor, sorcerer, novice_robe, 2, 5).
equipment(armor, sorcerer, apprentice_robe, 3, 8).
equipment(armor, sorcerer, expert_robe, 4, 12).



/* RULES */

equip(Item) :-
    search(Item),
    equipment(Type,ItemJob,Item,CombatValue,_),!,
    job(PlayerJob),
    equipped(Weapon,Armor),
    playerData(Lvl,HP,MaxHP,AttBef,DefBef,Exp,Gold),
    ItemJob == PlayerJob,
    deleteOne(Item),
    ((
    Type == weapon,
    Weapon == none,!,
    AttAft is AttBef + CombatValue,
    retract(equipped(_,_)),
    asserta(equipped(Item,Armor)),
    retract(playerData(_,_,_,_,_,_,_)),
    asserta(playerData(Lvl,HP,MaxHP,AttAft,DefBef,Exp,Gold))
    );(
    Type == weapon,
    insertOne(Weapon),
    equipment(_,_,Weapon,PrevCombatValue,_),
    AttAft is AttBef - PrevCombatValue + CombatValue,
    retract(equipped(_,_)),
    asserta(equipped(Item,Armor)),
    retract(playerData(_,_,_,_,_,_,_)),
    asserta(playerData(Lvl,HP,MaxHP,AttAft,DefBef,Exp,Gold))
    );(
    Type == armor,
    Armor == none,!,
    retract(equipped(_,_)),
    asserta(equipped(Weapon,Item)),
    retract(playerData(_,_,_,_,_,_,_)),
    asserta(playerData(Lvl,HP,MaxHP,AttBef,DefAft,Exp,Gold))
    );(
    Type == armor,
    insertOne(Armor),
    equipment(_,_,Armor,PrevCombatValue,_),
    DefAft is DefBef - PrevCombatValue + CombatValue,
    retract(equipped(_,_)),
    asserta(equipped(Weapon,Item)),
    retract(playerData(_,_,_,_,_,_,_)),
    asserta(playerData(Lvl,HP,MaxHP,AttBef,DefAft,Exp,Gold))
    )).

remWeapon :-
    inventory(_,Capacity),
    equipped(Weapon,Armor),
    maxCapacity(MaxCap),
    Capacity < MaxCap,
    Weapon \== none,
    equipment(_,_,Weapon,CombatValue,_),!,
    playerData(Lvl,HP,MaxHP,AttBef,DefBef,Exp,Gold),
    AttAft is AttBef - CombatValue,
    retract(playerData(_,_,_,_,_,_,_)),
    asserta(playerData(Lvl,HP,MaxHP,AttAft,DefBef,Exp,Gold)),
    retract(equipped(_,_)),
    asserta(equipped(none,Armor)),
    insertOne(Weapon).

remArmor :-
    inventory(_,Capacity),
    equipped(Weapon,Armor),
    maxCapacity(MaxCap),
    Capacity < MaxCap,
    Armor \== none,
    equipment(_,_,_,Armor,CombatValue,_),!,
    playerData(Lvl,HP,MaxHP,AttBef,DefBef,Exp,Gold),
    DefAft is DefBef - CombatValue,
    retract(playerData(_,_,_,_,_,_,_)),
    asserta(playerData(Lvl,HP,MaxHP,AttBef,DefAft,Exp,Gold)),
    retract(equipped(_,_)),
    asserta(equipped(Weapon,none)),
    insertOne(Armor).

showEq :-
    equipped(Weapon,Armor),
    displayItemName(Weapon,X),
    displayItemName(Armor,Y),
    write('Your Equipment :'), nl,
    format('Weapon  : ~w', [X]) , nl,
    format('Armor   : ~w', [Y]), nl.

isEquipment(Item) :-
    equipment(_,_,Item,_,_).

equipmentType(Item, Type) :-
    equipmet(Type,_,Item,_,_).
