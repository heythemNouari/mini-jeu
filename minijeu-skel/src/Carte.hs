module Carte where 

import SDL

import qualified Data.Map.Strict as M

import Text.Read



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







-- ######## SHow ######il manque que d'ecrire ce qu'on a la fin dans un fichier text 
instance Show Case where
    show c = case c of 
                        Carte.Normal  -> "N"
                        Porte  NS  Fermee -> "|"
                        Porte  EO  Fermee -> "-"
                        Mur     -> "M"
                        Entree  -> "E"
                        Sortie  -> "S" 



instance Show Carte where
  show (Carte cartel carteh carte_contenu) =
   M.foldlWithKey (\ acc  coord casee -> acc ++ (show casee) ) "" carte_contenu
     

addLines :: Int -> String -> String
addLines _ [] = ""
addLines 0 s = let str = (head s : []) ++ "\n"
					          in str ++ (addLines 6 (tail s)) 
addLines n s =  (head s : []) ++ (addLines (n - 1) (tail s))

-- on peut la mettre aussi dans le main 
writeCarteInFile :: Carte -> FilePath-> IO ()
writeCarteInFile  c@(Carte l h m) fp = do 
		         	                          let str = addLines (l - 1) ( show c )
	         	 	                          writeFile fp str 
-- ######################################################
      -- Read 
-- ######################################################


-- ################ fin read a faire ########## 


initCarte :: Carte
initCarte = Carte 7 9 initLaby

initLaby :: M.Map Coord Case
initLaby = M.fromList [ ((C 0 0),Mur),((C 0 1),Mur ),((C 0 2),Mur),((C 0 3),Mur),((C 0 4),Mur),((C 0 5),Mur),((C 0 6),Mur),
		                    ((C 1 0),Mur) , ((C 1 1),Mur),((C 1 2),Mur),((C 1 3),Mur),((C 1 4),Mur),((C 1 5),Carte.Normal),((C 1 6),Sortie),
                        ((C 2 0),Mur) , ((C 2 1),Mur) ,((C 2 2),Mur) ,((C 2 3),Mur) ,((C 2 4),Carte.Normal),((C 2 5),Carte.Normal),((C 2 6),Mur),
                        ((C 3 0),Mur) , ((C 3 1),Mur) ,((C 3 2),Mur) ,((C 3 3),Mur) ,((C 3 4),Carte.Normal),((C 3 5),Mur),((C 3 6),Mur),
                        ((C 4 0),Mur) , ((C 4 1),Porte NS Fermee) ,((C 4 2),Mur) ,((C 4 3),Carte.Normal) ,((C 4 4),Carte.Normal),((C 4 5),Mur),((C 4 6),Mur),
                        ((C 5 0),Mur) , ((C 5 1),Carte.Normal) ,((C 5 2),Mur) ,((C 5 3),Carte.Normal) ,((C 5 4),Mur),((C 5 5),Mur),((C 5 6),Mur),
                        ((C 6 0),Mur) , ((C 6 1),Carte.Normal) ,((C 6 2),Mur) ,((C 6 3),Carte.Normal) ,((C 6 4),Mur),((C 6 5),Mur),((C 6 6),Mur),
                        ((C 7 0),Mur) , ((C 7 1),Carte.Normal) ,((C 7 2),Carte.Normal) ,((C 7 3),Carte.Normal) ,((C 7 4),Mur),((C 7 5),Mur),((C 7 6),Mur),
                        ((C 8 0),Mur) , ((C 8 1),Entree) ,((C 8 2),Mur) ,((C 8 3),Mur) ,((C 8 4),Mur),((C 8 5),Mur),((C 8 6),Mur)]
