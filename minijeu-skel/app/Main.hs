{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

import Foreign.C.Types (CInt (..) )

import SDL
import SDL.Time (time, delay)
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import Keyboard (Keyboard)
import qualified Keyboard as K

import qualified Debug.Trace as T

import Model (GameState)
import qualified Model as M

loadBackground :: Renderer-> FilePath -> TextureMap -> SpriteMap -> String -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap  id = do
  tmap' <- TM.loadTexture rdr path (TextureId id ) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId id) (S.mkArea 0 0 640 480)
  let smap' = SM.addSprite (SpriteId id) sprite smap
  return (tmap', smap')

loadPerso :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 100 100)
  let smap' = SM.addSprite (SpriteId "perso") sprite smap
  return (tmap', smap')


loadObj :: Renderer-> FilePath -> TextureMap -> SpriteMap -> String -> IO (TextureMap, SpriteMap)
loadObj rdr path tmap smap id = do
  tmap' <- TM.loadTexture rdr path (TextureId id ) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId id ) (S.mkArea 0 0 80 80)
  let smap' = SM.addSprite (SpriteId id ) sprite smap
  return (tmap', smap')

chargement :: Renderer -> FilePath -> TextureMap ->  SpriteMap -> Int -> String -> IO (TextureMap, SpriteMap)
chargement renderer fp tmap smap 1 id = do 
                                         (tmap', smap') <-loadObj renderer fp tmap smap id
                                         return (tmap', smap')
chargement renderer fp tmap smap n id = do 
                                        (tmap', smap') <- loadObj renderer fp tmap smap (id ++ show(n))
                                         
                                        (tmap2, smap2) <- chargement renderer fp tmap' smap' (n-1) (id ++ show(n - 1))
                                        return (tmap2, smap2)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 640 480 }
  renderer <- createRenderer window (-1) defaultRenderer
  
  -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assets/background.jpg" TM.createTextureMap SM.createSpriteMap "background"
  -- chargement du personnage
  (tmap', smap') <- loadPerso renderer "assets/pers.png" tmap smap
  -- chargement des briques
  (tmap3, smap3) <- chargement renderer "assets/brick-wall.png" tmap' smap' 2 "brique"
  (tmap4, smap4) <- chargement renderer "assets/tresor.png" tmap3 smap3 1 "tresor"
  (tm, sm) <- loadBackground renderer "assets/win.jpg" tmap4 smap4 "win"

  
  -- initialisation de l'état du jeu
  let gameState = M.initGameState
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- lancement de la gameLoop
  gameLoop 60 renderer tm sm kbd gameState

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> GameState -> IO ()
gameLoop frameRate renderer tmap smap kbd gameState = do
  startTime <- time
  events <- pollEvents
 
  let kbd' = K.handleEvents events kbd
  
  clear renderer
           
  
  if (M.gameWin gameState)
     then  do
            putStrLn "rani switchit "
           
            S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "win") smap) 
            gameLoop frameRate renderer tmap  smap kbd' gameState
     else  do
            
            --- display background
            S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
            --- display perso 
            S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap)
                                          (fromIntegral (M.persoX gameState))
                                          (fromIntegral (M.persoY gameState)))

            --- display tresor
            S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "tresor") smap)
                                          (fromIntegral (M.tresorX gameState))
                                          (fromIntegral (M.tresorY gameState)))
          
            --- display briques 
            S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "brique1") smap)
                                          (50)
                                          (0))
            
            S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "brique2") smap)
                                          (0)
                                          (0 ))
            
            ---
            present renderer
            endTime <- time
            let refreshTime = endTime - startTime
            let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
            threadDelay $ delayTime * 1000 -- microseconds
            endTime <- time
            let deltaTime = endTime - startTime
            -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
            -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
            putStrLn $ " persox :" <> (show( M.persoX gameState )) <> "pixels"
            ---update du game state
            let gameState' = M.gameStep gameState kbd' deltaTime
            ---
            
                  
            
              
            unless (K.keypressed KeycodeX kbd') (if (K.keypressed KeycodeQ kbd') 
                                                    then let gsL = M.moveLeft gameState' 
                                                            in  gameLoop frameRate renderer tmap smap kbd' gsL
                                                    else if(K.keypressed KeycodeD kbd') 
                                                              then let gsR = M.moveRight gameState' 
                                                                      in gameLoop frameRate renderer tmap smap kbd' gsR
                                                              else if(K.keypressed KeycodeZ kbd') 
                                                                        then let gsU = M.moveUp gameState' 
                                                                                in gameLoop frameRate renderer tmap smap kbd' gsU
                                                                        else if(K.keypressed KeycodeS kbd') 
                                                                                then let gsD = M.moveDown gameState' 
                                                                                        in gameLoop frameRate renderer tmap smap kbd' gsD
                                                    else  gameLoop frameRate renderer tmap smap kbd' gameState'
                                                )


                          