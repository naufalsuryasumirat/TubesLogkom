:- dynamic(player/2).
:- dynamic(encounter/1).
:- dynamic(started/1).

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
    printMap(12, 12), !.

map :-
    started(no), !,
    write('Anda belum start game'), nl.

/*--------------------------------------------------------------------------*/

/* WASD */
w :-
    encounter(no),
    player(X, Y),
    Y2 is Y + 1,
    X2 is X,
    Y2 =\= 11,
    (\+ tiles(X2, Y2)),
    write('Anda bergerak satu langkah ke Utara'), nl,
    asserta(player(X2, Y2)),
    retract(player(X, Y)), !,
    check_lock(X2,Y2), !.

w :-
    encounter(no),
    write('Anda tertabrak'), nl, !.

w :-
    encounter(yes),
    write('Anda sedang dalam battle'), nl.

a :-
    encounter(no),
    player(X, Y),
    X2 is X + 1,
    Y2 is Y,
    X2 =\= 11,
    (\+ tiles(X2, Y2)),
    write('Anda bergerak satu langkah ke Barat'), nl,
    asserta(player(X2, Y2)),
    retract(player(X, Y)), !,
    check_lock(X2,Y2), !.

a :-
    encounter(no),
    write('Anda tertabrak'), nl, !.

a :-
    encounter(yes),
    write('Anda sedang dalam battle'), nl.

s :-
    encounter(no),
    player(X, Y),
    Y2 is Y - 1,
    X2 is X,
    Y2 =\= 0,
    (\+ tiles(X2, Y2)),
    write('Anda bergerak satu langkah ke Selatan'), nl,
    asserta(player(X2, Y2)),
    retract(player(X, Y)), !,
    check_lock(X2,Y2), !.

s :-
    encounter(no),
    write('Anda tertabrak'), nl, !.

s :-
    encounter(yes),
    write('Anda sedang dalam battle'), nl.

d :-
    encounter(no),
    player(X, Y),
    X2 is X - 1,
    Y2 is Y,
    X2 =\= 0,
    (\+ tiles(X2, Y2)),
    write('Anda bergerak satu langkah ke Timur'), nl,
    asserta(player(X2, Y2)),
    retract(player(X, Y)), !,
    check_lock(X2,Y2), !.

d :-
    encounter(no),
    write('Anda tertabrak'), nl, !.

d :-
    encounter(yes),
    write('Anda sedang dalam battle'), nl.

/*--------------------------------------------------------------------------*/

/* Status Player 
playerData(Level, HP, MaxHP, Att, Def, Exp, Gold) :- */

playerData(15, 10, 20).
playerLVL(1, 0).
playerGold(0).

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
    write('Your status : '),  nl,
    write('Job     : '),printJob, nl,
    format('Level   : ~w', [Level]), nl,
    format('Health  : ~w/~w', [HP,MaxHP]), nl,
    format('Attack  : ~w', [Att]) , nl,
    format('Defense : ~w', [Def]), nl,
    format('Exp     : ~w', [Exp]), nl,
    format('Gold    : ~w', [Gold]),nl.


/*--------------------------------------------------------------------------*/
/* Enemy Status */
:- dynamic(slime/3).
:- dynamic(wolf/3).
:- dynamic(goblin/3).
slime(1, 1, 30).
wolf(5, 5, 30).
goblin(10, 5, 40).
/* Enemy Encounter */
encounter(no).

start_encounter :-
    encounter(no),
    retract(encounter(no)),
    asserta(encounter(yes)).

end_encounter :-
    encounter(yes),
    retract(encounter(yes)),
    asserta(encounter(no)).
    /* tambahin retract special_counter */

encounter_chance(X) :-
    between(1, 10, X),
    start_encounter,
    write('Anda bertemu dengan slime'), nl,
    asserta(encounter_slime(yes)),
    battle_menu,
    slime(Attack, Defense, HP),
    asserta(battle_slime(Attack, Defense, HP)).

encounter_chance(X) :-
    between(11, 15, X),
    start_encounter,
    write('Anda bertemu dengan wolf'), nl,
    battle_menu,
    asserta(encounter_wolf(yes)),
    wolf(Attack, Defense, HP),
    asserta(battle_wolf(Attack, Defense, HP)).

encounter_chance(X) :-
    between(16, 20, X),
    start_encounter,
    write('Anda bertemu dengan goblin'), nl,
    battle_menu,
    asserta(encounter_goblin(yes)),
    goblin(Attack, Defense, HP),
    asserta(battle_goblin(Attack, Defense, HP)).

encounter_chance(101) :-
    start_encounter,
    write('Anda bertemu dengan bosNaga'), nl.

check_lock(X, Y) :-
    \+ store(X, Y),
    \+ quest(X, Y),
    \+ bosNaga(X, Y),
    random(1, 101, Z),
    encounter_chance(Z).

check_lock(X,Y) :-
    store(X,Y), !,
    write('Anda berada dalam Store, mau beli apa?'), nl.

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
    write('Run?'), nl.

/*--------------------------------------------------------------------------*/
/* TEST*/
:- dynamic(special_counter/1).
player(10, 10, 40). %Swordsman
special_counter(0).
:- dynamic(battle_goblin/3).
:- dynamic(battle_slime/3).
:- dynamic(battle_wolf/3).
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
    write('Run gagal, turn diberikan ke musuh'), nl.

run_success(X) :-
    between(1, 4, X),
    write('Run berhasil'), nl,
    end_encounter.

/* Ganti player() dengan fakta yang baru */
/* Tambah ATTACK dari enemy */
/* Attacking Slime */
attack :-
    encounter(yes),
    battle_slime(Attack, Defense, HP), !,
    player(AttackP, _, _),
    AttDealt is AttackP - Defense,
    NewHP is HP - AttackP + Defense,
    format('You dealt ~w damage to the Slime', [AttDealt]), nl,
    retract(battle_slime(_, _, _)),
    asserta(battle_slime(Attack, Defense, NewHP)),
    check_death_slime,
    special_counter(Count),
    0 =\= mod(Count, 3), !,
    special_increment.
    
/* Attacking Wolf */
attack :-
    encounter(yes),
    battle_wolf(Attack, Defense, HP), !,
    player(AttackP, _, _),
    AttDealt is AttackP - Defense,
    NewHP is HP - AttackP + Defense,
    format('You dealt ~w damage to the Wolf', [AttDealt]), nl,
    retract(battle_wolf(_, _, _)),
    asserta(battle_wolf(Attack, Defense, NewHP)),
    check_death_wolf,
    0 =\= mod(Count, 3), !,
    special_increment.

/* Attacking Goblin */
attack :-
    encounter(yes),
    battle_goblin(Attack, Defense, HP), !,
    player(AttackP, _, _),
    AttDealt is AttackP - Defense,
    NewHP is HP - AttackP + Defense,
    format('You dealt ~w damage to the Goblin', [AttDealt]), nl,
    retract(battle_goblin(_, _, _)),
    asserta(battle_goblin(Attack, Defense, NewHP)),
    check_death_goblin,
    0 =\= mod(Count, 3), !,
    special_increment.

/* CEK */
attack :-
    encounter(no),
    write('Anda tidak sedang di dalam battle'), nl.

check_death_slime :-
    battle_slime(_, _, X), !,
    X =< 0, !,
    retract(battle_slime(_, _, _)),
    /* Insert Player Level Here */
    /* Insert Player Gold Here */
    /* Inset Quest Counter Here */
    /* Reset Special Attack? */
    end_encounter,
    write('Slime defeated, great job!'), nl,
    write('You got 15 exp'), nl,
    write('You got 10 gold'), nl.

check_death_wolf :-
    battle_wolf(_, _, X), !,
    X =< 0, !,
    retract(battle_wolf(_, _, _)),
    /* Insert Player Level Here */
    /* Insert Player Gold Here */
    /* Inset Quest Counter Here */
    /* Reset Special Attack? */
    end_encounter,
    write('Wolf defeated, great job!'), nl,
    write('You got 20 exp'), nl,
    write('You got 50 gold'), nl.

check_death_goblin :-
    battle_goblin(_, _, X), !,
    X =< 0, !,
    retract(battle_goblin(_, _, _)),
    /* Insert Player Level Here */
    /* Insert Player Gold Here */
    /* Insert Quest Counter Here */
    /* Reset Special Attack? */
    end_encounter,
    write('Goblin defeated, great job!'), nl,
    write('You got 30 exp'), nl,
    write('You got 100 gold'), nl.

/* Slime Attack */
slime_attack :-
    battle_slime(Att, _, _).
    /* Insert Get Player Data Health */

/* Wolf Attack */
wolf_attack :-
    battle_wolf(Att,_,_).
    /* Insert Get Player Data Health */

/* Goblin Attack */
goblin_attack :-
    battle_goblin(Att, _, _).
    /* Insert Get Player Data Health */
    /* Calculate Damage Taken */

/* Add Enemy Attacking to Special Attack */

/* Special Attacking Slime */
specialAttack :-
    encounter(yes),
    player(AttP, _, _),
    battle_slime(Att, Def, HP), !,
    special_counter(Count),
    0 =:= mod(Count, 3), !,
    retract(battle_slime(_, _, _)),
    NewAttP is AttP * 3,
    AttDealt is NewAttP - Def,
    NewHP is HP - NewAttP + Def, 
    asserta(battle_slime(Att, Def, NewHP)),
    format('You dealt ~w damage to the Slime', [AttDealt]), nl,
    special_increment,
    check_death_slime.

/* Special Attacking Wolf */
specialAttack :-
    encounter(yes),
    player(AttP, _, _),
    battle_wolf(Att, Def, HP), !,
    special_counter(Count),
    0 =:= mod(Count, 3), !,
    retract(battle_wolf(_, _, _)),
    NewAttP is AttP * 3,
    AttDealt is NewAttP - Def,
    NewHP is HP - NewAttP + Def,
    format('You dealt ~w damage to the Wolf', [AttDealt]), nl,
    asserta(battle_wolf(Att, Def, NewHP)),
    special_increment,
    check_death_wolf.

/* Special Attacking Goblin */
specialAttack :-
    encounter(yes),
    player(AttP, _, _),
    battle_goblin(Att, Def, HP), !,
    special_counter(Count),
    0 =:= mod(Count, 3), !,
    retract(battle_goblin(_, _, _)),
    NewAttP is AttP * 3,
    AttDealt is NewAttP - Def,
    NewHP is HP - NewAttP + Def,
    format('You dealt ~w damage to the Goblin', [AttDealt]), nl,
    asserta(battle_goblin(Att, Def, NewHP)),
    special_increment,
    check_death_goblin.

specialAttack :-
    encounter(yes),
    player(AttP, _, _),
    special_counter(Count),
    X is mod(Count, 3),
    0 =\= mod(Count, 3), !,
    write('You have to wait ~w more turns to use special attack', [X]), nl.

check_special(X) :-
    0 =:= mod(X, 3), !.

check_special(X) :-
    Y is mod(X, 3),
    format('You have to wait ~w turns to use special attack', [Y]), fail.


/* tentuin begini apa engga pakenya ngurangin hp enemy */

special_increment :-
    special_counter(Count),
    retract(special_counter(_)),
    NewCount is Count + 1,
    asserta(special_counter(NewCount)).


/*--------------------------------------------------------------------------*/

/* Store */

/*--------------------------------------------------------------------------*/

/* Quest */

/*--------------------------------------------------------------------------*/