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
    ((JobNum = 1, asserta(job(swordsman)));
    (JobNum = 2, asserta(job(archer)));
    (JobNum = 3, asserta(job(sorcerer)))), 
    retract(started(no)),
    asserta(started(yes)),!.

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

map :- printMap(12, 12).

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
    write('Anda tertabrak'), nl.

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
    write('Anda tertabrak'), nl.

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
    write('Anda tertabrak'), nl.

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
    write('Anda tertabrak'), nl.

d :-
    encounter(yes),
    write('Anda sedang dalam battle'), nl.

/*--------------------------------------------------------------------------*/

/* Status Player */


/*--------------------------------------------------------------------------*/

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
    write('Anda bertemu dengan slime'), nl.

encounter_chance(X) :-
    between(11, 15, X),
    start_encounter,
    write('Anda bertemu dengan wolf'), nl.

encounter_chance(X) :-
    between(16, 20, X),
    start_encounter,
    write('Anda bertemu dengan goblin'), nl.

encounter_chance(101) :-
    start_encounter,
    write('Anda bertemu dengan bosNaga'), nl.

check_lock(X, Y) :-
    \+ store(X, Y),
    \+ quest(X, Y),
    \+ bosNaga(X, Y),
    random(1, 100, Z),
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

/*--------------------------------------------------------------------------*/

/* Battle Mechanism */

/*--------------------------------------------------------------------------*/

/* Store */

/*--------------------------------------------------------------------------*/

/* Quest */

/*--------------------------------------------------------------------------*/