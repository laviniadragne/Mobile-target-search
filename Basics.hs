{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List
import Data.Maybe

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}

data Game = Game {
      hunter :: Position
    , targets :: [Target]
    , obstacles :: [Position]
    , gateways :: [(Position, Position)]
    , nrlines :: Int
    , nrcolumns :: Int
} deriving (Eq, Ord)


{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}

{-
    Primeste o pozitie si un game si
    intoarce daca pozitia este hunterului din Game
-}
isHunter :: Position -> Game -> Bool
isHunter p (Game hun _ _ _ _ _) = p == hun

isTarget :: Position -> Game -> Bool
isTarget p (Game _ targetL _ _ _ _) = if (elem p [position mytarget | mytarget <- targetL]) then True else False

isObstacle :: Position -> Game -> Bool
isObstacle p (Game _ _ obstaclesL _ _ _) = (elem p obstaclesL)

isGateway :: Position -> Game -> Bool
isGateway p (Game _ _ _ gates _ _)
    | elem p [fst pos | pos <- gates] = True
    | elem p [snd pos | pos <- gates] = True
    | otherwise = False


isBlank :: Position -> Game -> Bool
isBlank p g
    | isHunter p g = False
    | isTarget p g = False
    | isObstacle p g = False
    | isGateway p g = False
    | otherwise = True


constructString :: Game -> Position -> Char
constructString g@(Game _ _ _ _ _ c) p
    | ((snd p) == c) = '\n'
    | (isHunter p g) = '!'
    | (isTarget p g) = '*'
    | (isObstacle p g) = '@'
    | (isGateway p g) = '#'
    | otherwise = ' '


gameAsString :: Game -> String
gameAsString g@(Game _ _ _ _ l c) = init (map (constructString g) [(x, y) | x <- [0 .. l - 1], y <- [0 .. c]])


instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}


emptyGame :: Int -> Int -> Game
emptyGame l c = Game { hunter = (1, 1),
                       targets = [],
                       obstacles = ([(x, 0) | x <- [0..l - 1]]
                                    ++ [(x, c - 1) | x <- [0..l - 1]]
                                    ++ [(0, x) | x <- [1..c - 2]]
                                    ++ [(l - 1, x) | x <- [1..c - 2]]),
                       gateways = [],
                       nrlines = l,
                       nrcolumns = c}

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}

addHunter :: Position -> Game -> Game
addHunter p g@(Game inithun _ _ _ l c) = newG where
    newG = g {hunter = newhunter}
    newhunter
        | ((fst p) < 0) = inithun
        | ((fst p) > (l - 1)) = inithun
        | ((snd p) < 0) = inithun
        | ((snd p) > (c - 1)) = inithun
        | (isBlank p g) = p
        | otherwise = inithun

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Target-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget b p g@(Game _ targetL _ _ _ _) = newG where
    newG = g {targets = (targetL ++ [newTarget])}
    newTarget = Target {position = p,
                        behavior = b}

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway (p1, p2) g@(Game _ _ _ gates _ _) = newG where
    newG = g {gateways = (gates ++ [newGate])}
    newGate = (p1, p2)

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle p g@(Game _ _ obstaclesL _ _ _) = newG where
    newG = g {obstacles = (obstaclesL ++ [p])}

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}


-- Intoarce gateway-ul pereche
retGateway :: Position -> Game -> Position
retGateway p (Game _ _ _ gates _ _)
    | (not (null (filter ((==p).fst) gates))) = (snd (head(filter ((==p).fst) gates)))
    | otherwise = (fst (head(filter ((==p).snd) gates)))

attemptMove :: Position -> Game -> Maybe Position
attemptMove p g
    | (isObstacle p g) = Nothing
    | (isGateway p g) = Just (retGateway p g)
    | otherwise = Just p

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

-- Verifica daca pozitia e valida
validPos :: Position -> Game -> Bool
validPos p g@(Game _ _ _ _ l c)
        | ((fst p) < 0) = False
        | ((fst p) > (l - 1)) = False
        | ((snd p) < 0) = False
        | ((snd p) > (c - 1)) = False
        | (isObstacle p g) = False
        | otherwise = True

-- Creeaza o noua pozitie
newPos :: Position -> Int -> Int -> Position
newPos p x y = newpos where
    newpos = ((fst p) + x, (snd p) + y)


goGeneral :: Int -> Int -> Behavior -> Behavior
goGeneral x y b p g = newTarget where
    newTarget = Target {position = validp,
                        behavior = b}
    validp
        | (isGateway (newPos p x y) g) = (retGateway (newPos p x y) g)
        -- daca nu am in fata un obstacol
        | (validPos (newPos p x y) g) = (newPos p x y)
        -- daca nu e valida pozitia urmatoare, dar sunt pe un gateway
        | ((not (validPos (newPos p x y) g)) && (isGateway p g)) = (retGateway p g)
        | otherwise = p


goEast :: Behavior
goEast p g = (goGeneral 0 1 goEast p g)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest p g = (goGeneral 0 (-1) goWest p g)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth p g = (goGeneral (-1) 0 goNorth p g)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth p g = (goGeneral 1 0 goSouth p g)

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}


bounce :: Int -> Behavior
bounce d p g

    -- daca unde vreau sa ma mut nu e un obstacol (e blank sau gateway) ma mut (pe nextpos sau pe gateway-ul pereche)                                             
    | ((attemptMove (newPos p d 0) g) /= Nothing) = Target {position = (fromJust (attemptMove (newPos p d 0) g)),
                                                            behavior = (bounce d)}
    | otherwise = Target {position = (newPos p (-d) 0),
                          behavior = (bounce (-d))}

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
-}


moveTargets :: Game -> Game
moveTargets  g@(Game _ targetL _ _ _ _)  = newG where
    newG = g {targets = newTargets}
    newTargets = [newT | t <- targetL, let newT = ((behavior t) (position t) g)]

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled p t
        | (newPos p 0 1) == (position t) = True
        | (newPos p 1 0) == (position t) = True
        | (newPos p 0 (-1)) == (position t) = True
        | (newPos p (-1) 0) == (position t) = True
        | otherwise = False



{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}

-- Intoarce pozitia hunterului dupa o mutare
moveHunter :: Position -> Int -> Int -> Game -> Position
moveHunter p x y g@(Game hun _ _ _ _ _) = newP where
    newP
        | (isGateway (newPos p x y) g) = (retGateway (newPos p x y) g)
        | (validPos (newPos p x y) g) = (newPos p x y)
        | otherwise = hun

convertDirInt :: Direction -> (Int, Int)
convertDirInt d
    | (d == North) = (-1, 0)
    | (d == South) = (1, 0)
    | (d == West) = (0, -1)
    | otherwise = (0, 1)

applyMoveHun :: Position -> Direction -> Game -> Position
applyMoveHun p d g = (moveHunter p (fst (convertDirInt d)) (snd (convertDirInt d)) g)


advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState d b g@(Game hun targetL _ _ _ _)
    | (not b) = g {hunter = (applyMoveHun hun d g)}
    | otherwise = g {hunter = (applyMoveHun hun d g),
                     targets = newTargets
                    }
                    where
    remainTargets = filter (\t -> if (isTargetKilled (applyMoveHun hun d g) t) then False else True) targetL
    movedTargets = targets (moveTargets g {hunter = (applyMoveHun hun d g),
                                           targets =  remainTargets})
    newTargets = filter (\t -> if (isTargetKilled (applyMoveHun hun d g) t) then False else True) movedTargets


{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft (Game _ targetL _ _ _ _)
    | not (null targetL) = True
    | otherwise = False


{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}

{-
    Genereaza toate pozitiile de pe un cerc
    (Se mentine distanta Manhattan constanta fata de C cercului)
    |x - x1| + |y - y1| = r
    pos = pozitia centrului (x1, y1)
    r = raza
-}
generate_pos :: Position -> Int -> [Position]
generate_pos pos r = [(x, y) | x <- [((fst pos) - r)..((fst pos) - 1)], let y = (snd pos) + (r - (abs ((fst pos) - x)))] ++
                     [(x, y) | x <- [(fst pos)..(((fst pos) + r) - 1)], let y = (snd pos) + (r - (abs ((fst pos) - x)))] ++
                     [(x, y) | x <- [((fst pos) + r), (((fst pos) + r) - 1) ..((fst pos) + 1)], let y = (snd pos) - (r - (abs ((fst pos) - x)))] ++
                     [(x, y) | x <- [(fst pos), ((fst pos) - 1) ..(((fst pos) - r) + 1)], let y = (snd pos) - (r - (abs ((fst pos) - x)))]

-- intoarce indexul pozitiei targetului din lista
get_index :: Position -> [Position] -> Int
get_index p c_pos = (fromJust (elemIndex p c_pos))

circle :: Position -> Int -> Behavior
circle centre r pos_target game = newTarget where
    index = (get_index pos_target) (generate_pos centre r)
    new_pos_target
        -- a iesit din lista, trebuie sa reia cercul
        | ((index + 1) > (4 * r - 1)) = ((generate_pos centre r) !! 0)
        | otherwise = ((generate_pos centre r) !! (index + 1))
    verify_pos
        -- next poz e un obstacol, va sta pe loc 
        | (isObstacle new_pos_target game) = pos_target
        | otherwise = new_pos_target
    newTarget = Target { position = verify_pos,
                         behavior = (circle centre r)}


instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game = [(North, (advanceGameState North False game))] ++
                      [(South, (advanceGameState South False game))] ++
                      [(West, (advanceGameState West False game))] ++
                      [(East, (advanceGameState East False game))]

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este una în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal (Game hun targetL _ _ _ _) = (not (null (filter (\x -> x == True) (map (isTargetKilled hun) targetL))))

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}


    h (Game hun targetL _ _ _ _) = (foldr min minAcc (map (hEuclidean hun) targetLPos)) where
        targetLPos = [newT | t <- targetL, let newT = (position t)]
        minAcc = 9223372036854775807

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Search este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors (BonusGame g) =  [(d, bg) | element <- (successors g), let d = (fst element), let bg = BonusGame (snd element)]

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal (BonusGame g) = (isGoal g)

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}

    -- distGH = distanta gateway-hunter
    -- distGT = distanta gateway-target
    h (BonusGame g) = (foldr min minAcc newList) where
        minAcc = 9223372036854775807
        newList = [distGH + distGT | gate <- (gateways g), let distGH = (hEuclidean (hunter g) (fst gate)),
                    let distGT = (hEuclidean (position (head (targets g))) (snd gate))] ++
                  [distGH + distGT | gate <- (gateways g), let distGH = (hEuclidean (hunter g) (snd gate)),
                    let distGT = (hEuclidean (position (head (targets g))) (fst gate))] ++ [(h g)]