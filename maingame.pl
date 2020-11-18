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
    started(no), !,
    write('Welcome to Genshin Asik. Choose your job'),nl,
    write('1. Swordsman'),nl,
    write('2. Archer'),nl,
    write('3. Sorcerer'),nl,
    read(JobNum),
    ((JobNum = 1, asserta(job(swordsman)), write('Anda memilih class Swordsman, good luck boi'), nl);
    (JobNum = 2, asserta(job(archer)), write('Anda memilih class Archer, good luck boi'), nl);
    (JobNum = 3, asserta(job(sorcerer)), write('Anda memilih class Sorcerer, good luck boi'), nl)), 
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

/* Status Player */
/* playerData([], [], [], [], []).
playerData([A|V], [B|W], [C,X], [D,Y], [E,Z]) :- */

playerName(nama).
playerData(15, 10, 20).
playerLVL(1, 0).
playerGold(0).

status :-
    write('Your status : '), nl,
    write('Job : '), nl,
    write('Health : '), nl,
    write('Attack : ') , nl,
    write('Defense : '), nl,
    write('Exp : '), nl,
    write('Gold : '),nl.


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
player(10, 10, 40). %Swordsman
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

attack :-
    encounter(yes),
    battle_slime(Attack, Defense, HP), !,
    player(AttackP, _, _),
    newHP is HP - AttackP + Defense,
    newAttack is Attack,
    newDefense is Defense,
    retract(battle_slime(_, _, _)),
    asserta(battle_slime(newAttack, newDefense, newHP)).
    /*newHP <= 0,
    write('Slime telah mati.'), nl,
    retract(battle_slime(_,_,_)),
    end_encounter.*/

attack :-
    encounter(yes),
    battle_goblin(Attack, Defense, HP), !,
    player(AttackP, _, _),
    newHP is HP - AttackP + Defense,
    retract(battle_goblin(_, _, _)),
    asserta(battle_goblin(Attack, Defense, newHP)).
    /*newHP <= 0,
    write('Goblin telah mati.'), nl,
    retract(battle_goblin(_,_,_)),
    end_encounter.*/
    

attack :-
    encounter(yes),
    battle_wolf(Attack, Defense, HP), !,
    player(AttackP, _, _),
    newHP is HP - AttackP + Defense,
    retract(battle_wolf(_, _, _)),
    asserta(battle_wolf(Attack, Defense, newHP)).
    /*newHP <= 0,
    write('Wolf telah mati.'), nl,
    retract(battle_wolf(_,_,_)),
    end_encounter.*/
/*--------------------------------------------------------------------------*/

/* Store */

/*--------------------------------------------------------------------------*/

/* Quest */

/*--------------------------------------------------------------------------*/