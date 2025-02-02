
module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K



data Modele = Cont {carte :: Carte , -- carte actuelle
                    envi :: Envi , -- environnement actuel
                    gene :: StdGen , -- generateur aleatoire
                    log :: String , -- journal d
                    keyboard :: Keyboard , -- l’etat du clavier
                    } 
bouge :: Modele -> Entite -> Coord -> Modele


moveLeft :: GameState -> GameState
moveLeft gs@(GameState px _ sp _ _) | px > 0 = gs { persoX = px - sp }
                                | otherwise = gs

moveRight :: GameState -> GameState
moveRight gs@(GameState px _ sp _ _) | px < 540 = gs { persoX = px + sp }
                                 | otherwise = gs

                              
moveUp :: GameState -> GameState
moveUp gs@(GameState _ py sp _ _) | py > 0 = gs { persoY = py - sp }
                              | otherwise = gs

moveDown :: GameState -> GameState
moveDown gs@(GameState _ py sp _ _) | py < 380 = gs { persoY = py + sp }
                                | otherwise = gs


gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate kbd deltaTime =
  -- A MODIFIFIER
  gstate
gameWin :: GameState -> Bool
gameWin gs@(GameState px py _ tx ty) = if (px == tx  && py == ty )
                                           then True 
                                           else False  