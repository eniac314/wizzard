import Control.Monad
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLP
import qualified Graphics.UI.SDL.Image as SDLI
import qualified Graphics.UI.SDL.Framerate as SDLF
import qualified Data.List as List
import System.Random
import System.Environment
import qualified Data.Vector as Vec
import Noise
import Tiles hiding (slow)
import Helper
import EngineTypes
import Vector
import Graphics hiding (nbrPts)
import Engine hiding (nbrPts)

{-# LANGUAGE BangPatterns #-}

{- Main -}

screenwidth = 672
screenheight = 672
nbrPts = 200

main = SDL.withInit [SDL.InitEverything] $ do
       
       fpsm <- SDLF.new
       SDLF.init fpsm
       
       gen <- getStdGen
       
       let go v = addThings v [(2,3,Building Tower),(45,23,Building SmallCastle2),(100,80,Being Mage4)]
           (oct,per,nbrSum,nbrCol) = (18, 0.2, 25, 7)
           (seed,_) = random gen
           --(seed,_) = random.mkStdGen $ 12
           
           --initial tile vector
           land = go $ vmap' noise2Tile (noiseMat oct per nbrPts nbrSum nbrCol seed)
           
           --width/height of displayed canvas (in tiles)
           canSize = 20

           --Starting position in chunk
           (canPosX,canPosY) = let off = (div nbrPts 2) - (div canSize 2) in (off,off)
       
       args <- getArgs
       
       scr <- SDL.setVideoMode screenwidth screenheight 32 [SDL.SWSurface]
       tilesData <- loadImage "./images/bigAlphaTiles.png"
       
       let system = Sys screenwidth screenheight fpsm
           current = Chunk Islands (canPosX,canPosY) land (canSize,canSize) nbrPts 0
           player' = Player (100,100) maje Stop
           world = World system scr tilesData current [] player'

       let loop w = 
            do let (t,s,fpsm) = (tileset w, screen w, fps.sys $ w)
                   (ch,(canX,canY)) = (chunk $ w, getCanvasPos $ w)
                   (wid,hei) = (width.sys $ w, height.sys $ w)
                   
               
               {- Rendering -}

               drawBackground wid hei s       
               applyTileMat w t s
               applyPlayer w t s
               SDL.flip s
               

               {- World Update -}
               let nextFrames = updateTail.updatePlayerTiles 
               let w' = moveCamera.movePlayer.nextFrames $ w
               
               event      <- SDL.pollEvent
               SDLF.delay fpsm

               --printPlayerData w'
                       
               case event of
                SDL.Quit -> return ()
                SDL.KeyDown (SDL.Keysym SDL.SDLK_LEFT _ _) -> loop $  changeDir w' Lefty
                SDL.KeyDown (SDL.Keysym SDL.SDLK_RIGHT _ _) -> loop $ changeDir w' Righty
                SDL.KeyDown (SDL.Keysym SDL.SDLK_UP _ _) -> loop $ changeDir w' Up
                SDL.KeyDown (SDL.Keysym SDL.SDLK_DOWN _ _) -> loop $ changeDir w' Down
                SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _) -> return ()
                SDL.KeyUp (SDL.Keysym _ _ _) -> loop $ changeDir w' Stop
                SDL.NoEvent -> loop w'
                _       -> loop w'
       
       loop world
