:- dynamic(player/2).
:- dynamic(encounter/1).
:- dynamic(started/1).
:- dynamic(inventory/2).

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
    printMap(12, 12), !.

map :-
    started(no), !,
    write('Anda belum start game'), nl.

/*--------------------------------------------------------------------------*/

/* WASD */
w :-
    started(yes),
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
    started(yes),
    encounter(no),
    write('Anda tertabrak'), nl, !.

w :-
    started(yes),
    encounter(yes),
    write('Anda sedang dalam battle'), nl.

w :-
    started(no),
    encounter(no),
    write('Anda belum memulai game'), nl, !.

a :-
    started(yes),
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
    started(yes),
    encounter(no),
    write('Anda tertabrak'), nl, !.

a :-
    started(yes),
    encounter(yes),
    write('Anda sedang dalam battle'), nl.

a :-
    started(no),
    encounter(no),
    write('Anda belum memulai game'), nl, !.

s :-
    started(yes),
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
    started(yes),
    encounter(no),
    write('Anda tertabrak'), nl, !.

s :-
    started(yes),
    encounter(yes),
    write('Anda sedang dalam battle'), nl.

s :-
    started(no),
    encounter(no),
    write('Anda belum memulai game'), nl, !.

d :-
    started(yes),
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
    started(yes),
    encounter(no),
    write('Anda tertabrak'), nl, !.

d :-
    started(yes),
    encounter(yes),
    write('Anda sedang dalam battle'), nl.

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
    ((retract(battle_slime(_,_,_))); (retract(battle_wolf(_,_,_))); (retract(battle_goblin(_,_,_)))).
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
    between(21, 30, X),
    start_encounter,
    write('Anda bertemu dengan wolf'), nl,
    battle_menu,
    wolf(_, Attack, Defense, HP),
    playerData(LVL, _, _, _, _, _, _),
    NewAttack is Attack + (LVL * 2),
    NewDefense is Defense + (LVL * 1),
    NewHP is HP + (LVL * 5),
    asserta(battle_wolf(NewAttack, NewDefense, NewHP)),
    status_enemy, !.

encounter_chance(X) :-
    between(31, 40, X),
    start_encounter,
    write('Anda bertemu dengan goblin'), nl,
    battle_menu,
    goblin(_, Attack, Defense, HP),
    playerData(LVL, _, _, _, _, _, _),
    NewAttack is Attack + (LVL * 2),
    NewDefense is Defense + (LVL * 2),
    NewHP is HP + (LVL * 7),
    asserta(battle_goblin(NewAttack, NewDefense, NewHP)),
    status_enemy, !.

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
    started(no),
    write('Anda belum mulai game'), nl, !.

status_enemy :-
    started(yes),
    encounter(no),
    write('Anda tidak sedang dalam battle'), nl, !.

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
    ((slime_attack);(wolf_attack);(slime_attack)).

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
    /* Inset Quest Counter Here */
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
    /* Inset Quest Counter Here */
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
    /* Insert Quest Counter Here */
    end_battle,
    write('Goblin defeated, great job!'), nl,
    add_gold(100),
    add_exp(30), !, fail.

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
    NewDef is Def + 2,
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
    asserta(playerData(LVL, NewHP, MHP, AttP, Def, Exp, Gold)).

slime_attack :-
    battle_slime(Att, _, _),
    playerData(LVL, HP, MHP, AttP, Def, Exp, Gold),
    AttDealt is Att - Def,
    AttDealt =< 0, !,
    write('You take no damage from the Slime'), nl.

wolf_attack :-
    battle_wolf(Att, _, _),
    playerData(LVL, HP, MHP, AttP, Def, Exp, Gold),
    AttDealt is Att - Def,
    AttDealt > 0, !,
    format('You take ~w damage from the Wolf', [AttDealt]), nl,
    NewHP is HP - AttDealt,
    retract(playerData(_, _, _, _, _, _, _)),
    asserta(playerData(LVL, NewHP, MHP, AttP, Def, Exp, Gold)).

wolf_attack :-
    battle_wolf(Att, _, _),
    playerData(LVL, HP, MHP, AttP, Def, Exp, Gold),
    AttDealt is Att - Def,
    AttDealt =< 0, !,
    write('You take no damage from the Wolf'), nl.

goblin_attack :-
    battle_goblin(Att, _, _),
    playerData(LVL, HP, MHP, AttP, Def, Exp, Gold),
    AttDealt is Att - Def,
    AttDealt > 0, !,
    format('You take ~w damage from the Goblin', [AttDealt]), nl,
    NewHP is HP - AttDealt,
    retract(playerData(_, _, _, _, _, _, _)),
    asserta(playerData(LVL, NewHP, MHP, AttP, Def, Exp, Gold)).

goblin_attack :-
    battle_goblin(Att, _, _),
    playerData(LVL, HP, MHP, AttP, Def, Exp, Gold),
    AttDealt is Att - Def,
    AttDealt =< 0, !,
    write('You take no damage from the Goblin'), nl.

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
    insertOne(Item).
insertPlenty(JumlahItem,Item) :-
    JumlahItem>1,
    insertOne(Item),
    Decr is JumlahItem-1,
    insertPlenty(Decr,Item).

/*insertOne untuk memasukkan 1 item ke inventory*/
insertOne(Item) :-
    inventory(Arr,Capacity),
    Capacity < 15,
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

rekurensFrontPopBack(ArrInvChecking,Front,PoppedArr,Nama1ItemInv) :-
    ArrInvChecking \== [],
    front(ArrInvChecking,Front),
    pop(ArrInvChecking,PoppedArr),
    back(Front,Nama1ItemInv).

%Basis
insRekursif(Item,[],ArrayPindahan,ResultAkhir) :-
    reverse(ArrayPindahan,RevArrayPindahan),
    push([1,Item],RevArrayPindahan,ResultAkhir),!.

%Rekurens
insRekursif(Item,ArrInvChecking,ArrayPindahan,ResultAkhir) :-
    rekurensFrontPopBack(ArrInvChecking,Front,PoppedArr,Nama1ItemInv),
    Item \== Nama1ItemInv,!,
    push(Front,ArrayPindahan,ArrayPindahanT),
    insRekursif(Item,PoppedArr,ArrayPindahanT,ResultAkhir).

insRekursif(Item,ArrInvChecking,ArrayPindahan,ResultAkhir) :-
    rekurensFrontPopBack(ArrInvChecking,Front,PoppedArr,Nama1ItemInv),
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
    JumlahItem > 1,
    inventory(Arr,Capacity),
    deleteOne(Item),
    Decr is JumlahItem-1,
    deletePlenty(Decr,Item).

/*deleteOne untuk memasukkan 1 item ke inventory*/
deleteOne(Item) :-
    inventory(Arr,Capacity),
    Capacity > 0,
    ArrInvChecking = Arr,
    delRekursif(Item,ArrInvChecking,[],ResultAkhir).
    
%Basis
delRekursif(Item,[],ArrayPindahan,ResultAkhir) :-
    reverse(ArrayPindahan,ResultAkhir).

%Rekurens
delRekursif(Item,ArrInvChecking,ArrayPindahan,ResultAkhir) :-
    rekurensFrontPopBack(ArrInvChecking,Front,PoppedArr,Nama1ItemInv),

    Item \== Nama1ItemInv,!,

    push(Front,ArrayPindahan,ArrayPindahanT),
    delRekursif(Item,PoppedArr,ArrayPindahanT,ResultAkhir).

delRekursif(Item,ArrInvChecking,ArrayPindahan,ResultAkhir) :-
    rekurensFrontPopBack(ArrInvChecking,Front,PoppedArr,Nama1ItemInv),

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

/* SEARCH */

search(Item) :-
    inventory(Arr,Capacity),
    ArrInvChecking = Arr,
    searchRekursif(Item,ArrInvChecking,[]).

searchRekursif(Item,[],ArrayPindahan) :-
    1 == 0.

searchRekursif(Item,ArrInvChecking,ArrayPindahan) :- 
    rekurensFrontPopBack(ArrInvChecking,Front,PoppedArr,Nama1ItemInv),
    Item \== Nama1ItemInv,!,
    push(Front,ArrayPindahan,ArrayPindahanT),
    searchRekursif(Item,PoppedArr,ArrayPindahanT).

searchRekursif(Item,ArrInvChecking,ArrayPindahan) :-
    rekurensFrontPopBack(ArrInvChecking,Front,PoppedArr,Nama1ItemInv),
    Item == Nama1ItemInv,!,
    1 == 1.