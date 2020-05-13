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







-- ######## SHow ######il manque que d'ecrire ce qu'on a la fin dans un fichier text 
instance Show Case where
    show c = case c of 
                        Normal  -> "N"
                        Porte Fermee NS   -> "|"
                        Porte Fermee EO   -> "-"
                        Mur     -> "M"
                        Entree  -> "E"
                        Sortie  -> "S" 

toList ::M.Map Coord Case -> [Case]
toList map = M.foldrWithKey (\_ a b -> a:b ) [] map


instance Show Carte where
  show (Carte cartel carteh carte_contenu) =
   M.foldlWithKey (\ acc  coord casee -> acc ++ (show casee) ) "" carte_contenu
     

addLines :: Int -> String -> String
addLines 0 s = if (S.strNull s ) then "" 
				  else let str = (head s : []) ++ "\n"
					in str ++ (addLines 6 (tail s)) 
addLines n s = if (S.strNull s ) then "" 
				  else (head s : []) ++ (addLines (n - 1) (tail s))

-- on peut la mettre aussi dans le main 
writeCarteInFile :: Carte -> FilePath-> IO ()
writeCarteInFile  c@(Carte l h m) fp = do 
		         	                          let str = addLines (l - 1) ( show c )
	         	 	                          writeFile fp str 
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


instance Read Case where
  readsPrec _ s = [((readCase (head(toListFromString s))),"")] 



 
readsPrec ::(Read a )=> Int -> String -> [(a,String)]
readsPrec _ c = [((readCase c),"")] 


instance Read Carte where 
  

-- ################ fin read a faire ########## 


initCarte :: Carte
initCarte = Carte 5 9 initLaby

initLaby :: M.Map Coord Case
initLaby = M.fromList [((C 0 0),Mur),((C 0 1),Mur ),((C 0 2),Entree),((C 0 3),Normal),((C 0 4),Mur),
		                    ((C 1 0),Mur) , ((C 1 1),Mur),((C 1 2),Mur),((C 1 3),Sortie),((C 1 4),Mur),
                        ((C 1 0),Mur) , ((C 1 0),Mur) ,((C 1 0),Mur) ,((C 1 0),Mur) ,((C 1 0),Mur),
                        ((C 1 0),Mur) , ((C 1 0),Mur) ,((C 1 0),Mur) ,((C 1 0),Mur) ,((C 1 0),Mur),
                        ((C 1 0),Mur) , ((C 1 0),Mur) ,((C 1 0),Mur) ,((C 1 0),Mur) ,((C 1 0),Mur),
                        ((C 1 0),Mur) , ((C 1 0),Mur) ,((C 1 0),Mur) ,((C 1 0),Mur) ,((C 1 0),Mur),
                        ((C 1 0),Mur) , ((C 1 0),Mur) ,((C 1 0),Mur) ,((C 1 0),Mur) ,((C 1 0),Mur),
                        ((C 1 0),Mur) , ((C 1 0),Mur) ,((C 1 0),Mur) ,((C 1 0),Mur) ,((C 1 0),Mur),
                        ((C 1 0),Mur) , ((C 1 0),Mur) ,((C 1 0),Mur) ,((C 1 0),Mur) ,((C 1 0),Mur)]
