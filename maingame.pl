:- dynamic(player/2).
:- dynamic(encounter/1).
:- dynamic(started/1).
:- dynamic(inventory/2).
:- dynamic(equipped/2).
:- dynamic(job/1).
:- dynamic(playerData/7).

tiles(4,4).
tiles(5,4).
tiles(6,4).
tiles(4,5).
tiles(4,6).

player(9,9).

equipped(none,none).
bosNaga(1,1).
store(9,7).
quest(1,10).

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
    read(JobNum),!,
    ((
    JobNum == 1,!,
    asserta(job(swordsman)),
    asserta(playerData(1,100,100,10,6,0,0)),
    asserta(inventory([[1,wooden_sword]],1)),
    equip(wooden_sword),
    write('Anda memilih class Swordsman, good luck boi'), 
    nl);
    (
    JobNum == 2,!,
    asserta(job(archer)),
    asserta(playerData(1,80,80,12,4,0,0)),
    asserta(inventory([[1,wooden_bow]],1)),
    equip(wooden_bow),
    write('Anda memilih class Archer, good luck boi'), 
    nl);
    (
    JobNum == 3,!,
    asserta(job(sorcerer)), 
    asserta(playerData(1,60,60,15,3,0,0)),
    asserta(inventory([[1,flame_staff]],1)),
    equip(flame_staff),
    write('Anda memilih class Sorcerer, good luck boi'), 
    nl
    )),
    insertPlenty(5,healing_potion),
    retract(started(no)),
    asserta(started(yes)).
    
    %map.

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
/* Attacking Slime */
attack :-
    encounter(yes),
    battle_slime(Attack, Defense, HP), !,
    player(AttackP, _, _),
    NewHP is HP - AttackP + Defense,
    NewAttack is Attack,
    NewDefense is Defense,
    retract(battle_slime(_, _, _)),
    asserta(battle_slime(NewAttack, NewDefense, NewHP)),
    check_death_slime.
    /*newHP <= 0,
    write('Slime telah mati.'), nl,
    retract(battle_slime(_,_,_)),
    end_encounter.*/
    
/* Attacking Wolf */
attack :-
    encounter(yes),
    battle_wolf(Attack, Defense, HP), !,
    player(AttackP, _, _),
    NewHP is HP - AttackP + Defense,
    retract(battle_wolf(_, _, _)),
    asserta(battle_wolf(Attack, Defense, NewHP)),
    check_death_wolf.
    /*newHP <= 0,
    write('Wolf telah mati.'), nl,
    retract(battle_wolf(_,_,_)),
    end_encounter.*/

/* Attacking Goblin */
attack :-
    encounter(yes),
    battle_goblin(Attack, Defense, HP), !,
    player(AttackP, _, _),
    NewHP is HP - AttackP + Defense,
    retract(battle_goblin(_, _, _)),
    asserta(battle_goblin(Attack, Defense, NewHP)),
    check_death_goblin.
    /*newHP <= 0,
    write('Goblin telah mati.'), nl,
    retract(battle_goblin(_,_,_)),
    end_encounter.*/

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

specialAttack :-
    encounter(yes),
    special_counter(Count),
    0 =:= mod(Count, 3), !.
    


/*--------------------------------------------------------------------------*/

/* Store */

/*--------------------------------------------------------------------------*/

/* Quest */

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

remeWeapon :-
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
    write('Your Equpment :'), nl,
    format('Weapon  : ~w', [X]) , nl,
    format('Armor   : ~w', [Y]), nl.

isEquipment(Item) :-
    equipment(_,_,Item,_,_).

equipmentType(Item, Type) :-
    equipmet(Type,_,Item,_,_).
