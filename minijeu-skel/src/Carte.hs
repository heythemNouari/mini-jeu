module Carte where 

import SDL

import qualified Data.Map.Strict as M

import Text.Read

import qualified Data.Matrix as MT 

 data PDirection = NS | EO deriving Eq -- direction d’une porte

 data StatutP = Ouverte | Fermee deriving Eq -- statut d’une porte

data Case = Normal -- une case vide
        | Porte  PDirection StatutP -- une porte ouverte ou fermee
        | Mur -- infranchissable (sauf pour les fantomes ...)
        | Entree -- debut du niveau
        | Sortie -- fin du niveau
        deriving Eq

data Coord = C {cx :: Int , cy :: Int} deriving (Eq, Ord)

data Carte = Carte { cartel :: Int , 
                     carteh :: Int , 
                     carte_contenu :: (M.Map Coord Case) -- cases de la carte
                    }


instance Show Case where
    show c = case c of 
                        Normal  -> "N"
                        Porte Fermee NS   -> "|"
                        Porte Fermee EO   -> "-"
                        Mur     -> "M"
                        Entree  -> "E"
                        Sortie  -> "S" 

-- Readspc entee 0 =[ ( tafonction entree,entre)]
 
 

-- foldWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b !! 
-- map :: (a -> b) -> Map k a -> Map k b !! 
 -- utlise c deux la le map prend une map de k a et te renvoie une map de k b car pour chaque case elle applique la fonction 
-- fold :: (a -> b -> b) -> b -> Map k a -> b

-- exemple t'as une map [(0,0),(1,1)]
-- si je fait M.map (\valeur ->  valeur +1 ) map =====> sa me renvoie [(0,1),(1,2)]
-- okay okay i see
-- foldWi.. celle la c pour changer d'accumulateur genre tu veux pas retourner une map 

-- exemple map [(0,0),(1,1)]
-- M.foldWithKey(\ clef valeur accumulateur -> valeur + accumulateur ) 0 map ====>  renvoie 1 car elle fait 0 + 1
-- 0 + 0 = 0 + 1 = 1 
-- non la j utilister que les valeurs oui l'accumulateur au debut 0 ydi oui oui oui mdr oui 
-- donc surement nestekhdemha f read COrte
-- oui ses deux fnction iteb3ouk partout 

-- oui c juste faut utiliser berk show et read plus les foncteur ( map foldr .....) pour prouver que ta compris le cote fonctionnel




-- ######## SHowil manque que d'ecrire ce qu'on a la fin dans un fichier text 

toList ::M.Map Coord Case -> [Case]
toList map = M.foldrWithKey (\_ a b -> a:b ) [] map

-- j'ai importe Matrix en MT et j fait appela  cette fonction 
--  MT.prettyMatrix  (MT.fromList ch(heuteur ) cl(largeur ) (toList contenu) (sa doit etre une liste de cases berk ))
instance Show Carte where
  show (Carte cartel carteh carte_contenu) =
    MT.prettyMatrix  (MT.fromList ch cl (toList carte_contenu))
    -- M.foldWithKey (\coord casee accummulateur -> accummulateur ++ (show casee) ) "" carte_contenu 


-- ######################################################
      -- Read 
-- ######################################################

readCase :: Char -> Case
readCase '|' = (Porte EO Fermee) 
readCase '-' = (Porte NS Fermee) 
readCase 'X' = Mur 
readCase 'E' = Entree
readCase 'S' = Sortie   

readCase  _  = Carte.Normal 

-- quand tu instancie read tu dois definir la fonction readsPrec qui est de cette definition 
-- reasdPrec ::(Read a )=> Int -> String -> [(a,String)]
-- le Int tu t'en fout tu donne en paramettre un string  et tu appel la fonction qui cree une carte a partir d'un string 
instance Read Case where
  readsPrec _ s = [((readCase (head(toListFromString s))),"")] 


-- read case prend un car et me renvoie une case du coup  ma  fonction reasPrec me renvoie qu'un seul couple 
-- [(case,string )] ou moi j'ignore le string et je prends que la case 
-- tu ma compris a peurpes ? 
-- oui juste une question : il faut modifie win rani deyer map nn ? nradja3ha matrix
-- non c just epour l'affichage que je l'utlise 
-- tu utilises tes map normale jsute pour l'affichage car ya un fonction deja predefini de matrix qui me renvoie 
 
readsPrec ::(Read a )=> Int -> String -> [(a,String)]
readsPrec _ c = [((readCase c),"")] 


instance Read Carte where 
  

-- ################ fin read a faire ########## 


initCarte :: Carte
initCarte = Carte 7 9 initLaby

initLaby :: M.Map Coord Case
initLaby = M.fromList [((C 0 0),Mur),((C 0 1),Porte NS Ouverte ),((C 0 2),Entree),((C 0 3),Normal),((C 0 4),Mur),
		       ((C 1 0),Mur) , ((C 1 1),Mur),((C 1 2),Mur),((C 1 3),Sortie),((C 1 4),Mur)]
