module Entite where 

import SDL



data Entite = Monstre {iden :: Int }
            | Joueur {iden :: Int }
            deriving (Eq)