{
type cellule <- enregistrement
    x & y   :ENTIER
FINENREGISTREMENT

TYPE

    bateau : tableau de 1 a n de cellule
    
    
    Bataille navale :
Réaliser l'algo et le Pascal du jeu la bataille navale. Vous utilierez pour représenter la grille du jeu un type record nommé case afin de décrire les cases occupées par les bateaux. De meme, un bateau sera décris par un enxemble de cases et la flotte de bateau à couler sera representée par un ensemle de bateau.

Travail a faire : 
1. Ecrire un type enregistrement case composé de deux champs ; ligne et colonne de type  entiers
2. Ecrire un type bateau composé d'un ensemble de n cases
3. Ecrire la structure flotte composé d'un ensemble de bateaux.
4. Ecrire une fonction de création d'une case. Elle prends en paramètre la ligne et la colonn associée a la case. 
5. Ecrire une fonction de comparaison de deux case. Elle renverra vrai ou faux selon le cas.
6. Ecrire une fonction de création de bateaux, elle renverra un type enregistrement bateau correctement remplie.
7. Ecrire un fonction qui vérifie qu'un case appartient a un bateau . Elle renvera vrai ou faux selon le cas. Cette fonction de verfication devra utiliser votre fonction de comparaison de case.
8. Ecrire une fonction qui vérifie qu'un case appartient a une flotte de bateau. Cette fonction devra utiliser votre fonction de verification pour un bateau.
9. Bonne chance. Programme complet.	


programme Bataille

TYPE case
DEBUTENREGISTREMENT

x,y  : entier
FINENREGISTREMENET

TYPE tabbateau <- tableau de 1 a 3 de case

type BATEAU
DEBUTENREGISTREMENT
    tabBateau      <- tabBateau
FINENREGISTREMENT


TYPE tabflotte <- tableau de 1 a 4 de bateau

type flotte
DEBUTENREGISTREMENT
    tabflotte <- tabflotte
FINENREGISTREMENT


fonction comparerCase(case1 : case, case2 : case)   :booléen        //On compare le contenu de deux cases. Si il est égal, on revoie Vrai. Sinon, on renvoie faux.
DEBUTFONCTION
    SI case1 = case2 ALORS
        comparerCase = VRAI
    SINON
        comparerCase = FAUX
FINFONCTION


fonction creerBateau( numero : entier)      :bateau
VAR
    boat    :bateau;
    i       :entier;
    
DEBUTFONCTION
    SI numero = 1 ALORS
        POUR i de 1 a 3 FAIRE
            boat.tabbateau[i].x = 3;
        POUR i de 1 a 3 FAIRE               //On créé le bateau 1 commençant en 3.2 et finissant en 3.4
            boat.tabbateau[i].y = i+1;
            
    SI numero = 2 ALORS
        POUR i de 1 a 3 FAIRE
            boat.tabbateau[i].x = 1;
        POUR i de 1 a 3 FAIRE              //On créé le bateau 2 commençant en 1.8 et finissant en 1.10
            boat.tabbateau[i].y = 7+i;
            
    SI numero = 3 ALORS
        POUR i de 1 a 3 FAIRE
            boat.tabbateau[i].x = i+4;
        POUR i de 1 a 3 FAIRE               //On créé le bateau 3 commençant en 3.2 et finissant en 3.4
            boat.tabbateau[i].y = 8;
            
    SI numero = 4 ALORS
        POUR i de 1 a 3 FAIRE
            boat.tabbateau[i].x = 9;
        POUR i de 1 a 3 FAIRE               //On créé le bateau 4 commençant en 3.2 et finissant en 3.4
            boat.tabbateau[i].y = i+3;
FINFONCTION

fonction checkBateau (case : case, flotte : flotte);

VAR
i,j     :entier;
DEBUTFONCTION
    pour i de 1 a 4
        pour j de 1 a 3
           SI comparerCase(case.y,flotte.tabFlotte[i].tabBateau[j].y) ET comparerCase(case.x,flotte.tabFlotte[i].tabBateau[j].x) ALORS
                checkbateau <- VRAI;                    //On ultilise la fonction compare pour voir si la case entrée est la meme qu'un des cases occupés par un des bateaux de la flotte.
        FINPOUR                                         //Si c'est le cas, on renvoie vrai, sinon, on revoie faux.
    FINPOUR
checkbateau <- FAUX;
FINFONCTION

POGRAMME bataille
VAR
flottejoueur        :flotte;
xentre,yentre,i,hits     :entier;
casentre            :case;
bonnetnre           :array [1..12] of integer   //Ce tableau sert a stocker les bonnes coordonnées que le joueur a entré pour pouvoir éviter qu'il ne les mettents un deuxieme fois
                                                //Elles sont stockés sous la forme x*10+y.

DEBUT
hits <- 1
pour i e 1 a 4 faire
    flotte.tabflotte[i] <- creerBateau[i];   //On créé les bateaux et on les stock dnas la flotte
FINPOUR

repeat

    repeat
        ECRIRE "N'entrez pas une valeure que vous avez deja entré."
        repeat
            ECRIRE "Veuillez entrer la coordonnée x."               //On demande les ccordoné a l'utilisateur. On n'accepte l'input que si
            LIRE xentre                                             //il est entre 1 et 10 et si il ne correspond pas a un input ayant touché auparavant.
        until xentre =< 10 ET xentre => 10
        
        repeat
            ECRIRE "Veuillez entrer la coordonée y."
            lire yentre
        until yentre =< 10 ET yentre => 10
        
    until xentre*10+yentre <> bonnentre[1..12]                      
    casentre.x <- xentre
    casentre.y <- yentre
    SI checkBateau(casentre,flottejoueur) = VRAI ALORS
        bonnentre[hits] <- xentre*10+yentre                         //On met les coord rentrées dans le tableau pour éviter qu'elles soient rentrées  nouveau.
        hits <- hits + 1
        

until hits = 12             //On répète la boucle principale jusqu'a qu'on est touché tout les zones touchables


FIN.
}

program Bataille;

uses crt;

TYPE cell = record

    x,y : integer;
END;
    
TYPE tabbateau = array [1..3] of cell;



TYPE bateau = record
    
    bateau : tabbateau;
END;

TYPE tabflotte = array [1..4] of bateau;

TYPE flotte = record

    flotte : tabflotte;
END;

function    compareCell(cell1 : integer;cell2 : integer) : boolean;
    BEGIN   
            if cell1 = cell2 then
                compareCell := true
            else
                compareCell := false;
    END;

function    createBateau(numero : integer)    :bateau;
    VAR
    i    :   integer;
    lclBoat        :   tabBateau;
    boat            : bateau;
    BEGIN
    if numero = 1 then
    BEGIN
        for i := 1 to 3 do
        BEGIN
            boat := bateau.tabBateau[i].x := 3;
        END;
        for i := 1 to 3 do
        BEGIN
            boat := bateau.tabBateau[i].y := 1+i;
        END;
        createBateau := boat;
    END;
    else if numero = 2 then
    else if numero = 3 then
    else if numero = 4 then
        
    END;

VAR

    flotte                            =   flotte;
    bateau1                           =   bateau;
    i,j                               =   integer;

BEGIN
    bateau1 := createBateau(1);
    for i := 1 to 3 do
        readln(bateau1.tabBateau[i].x);
        readln(bateau1.tabBateau[i].y);
END.


flotte[1].bateau[3].x



