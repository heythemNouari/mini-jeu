module Environnement where

import SDL
import qualified Data.Map as M
import Data.List (foldl')


import Entite (Entite)
import Data.Set (Set)
import qualified Data.Set as S

data Envi = Envi { contenu_envi :: M.Map Coord [ Entite ]}


franchissable_env :: Coord -> Envi -> Bool
franchissable_env coord (env cnt) = if (contenu M.! coord ) == Monstre x ) then false 
                                                                    else true


-- a refaire 

trouve_id :: Int -> Envi -> Maybe (Coord, Entite)
trouve_id x env = if ((findWithDefault (Joueur 50) x) == (Joueur 50)) then Nothing
                                                                      else Just (findWithDefault (Joueur 50) x)

rm_env_id :: Int -> Envi -> Envi

bouge_id :: Int -> Coord -> Envi -> Envi