import Control.Monad
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLP
import qualified Graphics.UI.SDL.Image as SDLI
import qualified Graphics.UI.SDL.Framerate as SDLF
import qualified Graphics.UI.SDL.Mixer as SDLM
import qualified Data.List as List
import System.Random
import System.Environment
import qualified Data.Vector as Vec
import Noise
import Tiles hiding (slow)
import Helper
import EngineTypes
import Vector
import Graphics 
import Engine
import WorldUpdates
import Control.DeepSeq
--import Euterpea

{-# LANGUAGE BangPatterns #-}

--t251 :: Music Pitch
--t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
--           gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
--           cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
--       in dMinor :+: gMajor :+: cMajor

{- Main -}

screenwidth = 672
screenheight = 672
nbrPts = 200
chunkType = Continent
main = SDL.withInit [SDL.InitEverything] $ do
       
       fpsm <- SDLF.new
       SDLF.init fpsm
       SDL.enableKeyRepeat 200 0 -- (initial delay, delay between each key presses)
       
       gen <- getStdGen
       
       let --go w = addThings w [(15,15,Building SmallCastle2),(45,25,Building SmallCastle2)]
           (seed,_) = random gen
           --(seed,_) = random.mkStdGen $ 12
           
           --initial tile vector
           land = makeLand chunkType nbrPts seed
           
           --width/height of displayed canvas (in tiles)
           canSize = 20 

           (plX,plY) = let off = (div nbrPts 2) in (off,off)

           --Canvas origin position in chunk
           (canPosX,canPosY) = (plX - (div canSize 2),plY - (div canSize 2)) 
       
       args <- getArgs
       
       scr <- SDL.setVideoMode screenwidth screenheight 32 [SDL.SWSurface]
       tilesData <- loadImage "./images/bigAlphaTiles.png"
       
       let !system = Sys screenwidth screenheight fpsm
           !current = Chunk chunkType (canPosX,canPosY) land (canSize,canSize) nbrPts 0
           !player' = Avatar (plX,plY) maje Stop
           !world = (addVarious seed).addBorders $ World system scr tilesData current [] player'
       

       let loop w = 
            do let (t,s,fpsm) = (tileset w, screen w, fps.sys $ w)
                   (ch,(canX,canY)) = (chunk $ w, getCanvasPos $ w)
                   (wid,hei) = (width.sys $ w, height.sys $ w)
                   
               
               {- Rendering -}

               drawBackground wid hei s       
               applyTileMat w t s
               applyPlayer w t s
               --forM_ [1..100] (\i -> drawAlphaPoly (0,0) wid hei (0,0,150,i) s >> SDL.flip s >> SDL.delay 10)
               SDL.flip s
               

               {- World Update -}
               let nextFrames = updateTail.updatePlayerTiles 
               let w' =  moveCamera.movePlayer.nextFrames $ w
               
               event      <- SDL.pollEvent
               SDLF.delay fpsm

               --printPlayerData w'
                       
               case event of
                SDL.Quit -> return ()
                SDL.KeyDown (SDL.Keysym SDL.SDLK_LEFT _ _) -> loop $  changeDir w' Lefty
                SDL.KeyDown (SDL.Keysym SDL.SDLK_RIGHT _ _) -> loop $ changeDir w' Righty
                SDL.KeyDown (SDL.Keysym SDL.SDLK_UP _ _) -> loop $ changeDir w' Up
                SDL.KeyDown (SDL.Keysym SDL.SDLK_DOWN _ _) -> loop $ changeDir w' Down
                SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _) -> loop w'
                SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _) -> return ()
                --SDL.KeyUp (SDL.Keysym _ _ _) -> loop $ changeDir w' Stop
                SDL.NoEvent -> loop $ changeDir w' Stop
                _       -> loop w'
       
       
       loop world
