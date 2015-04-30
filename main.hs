import Control.Monad
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLP
import qualified Graphics.UI.SDL.Image as SDLI
import qualified Graphics.UI.SDL.Framerate as SDLF
import Graphics.UI.SDL.Mixer
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
import Control.DeepSeq

{-# LANGUAGE BangPatterns #-}

{- Main -}

screenwidth = 672
screenheight = 672
nbrPts = 200

main = SDL.withInit [SDL.InitEverything] $ do
       
       fpsm <- SDLF.new
       SDLF.init fpsm
       SDL.enableKeyRepeat 200 0 -- (initial delay, delay between each key presses)
       
       gen <- getStdGen
       
       let --go w = addThings w [(15,15,Building SmallCastle2),(45,25,Building SmallCastle2)]
           (oct,per,nbrSum,nbrCol) = (18, 0.2, 25, 7)
           (seed,_) = random gen
           --(seed,_) = random.mkStdGen $ 12
           
           --initial tile vector
           land = vmap' noise2Tile (noiseMat oct per nbrPts nbrSum nbrCol seed)
           
           --width/height of displayed canvas (in tiles)
           canSize = 20 

           (plX,plY) = let off = (div nbrPts 2) in (off,off)

           --Canvas origin position in chunk
           (canPosX,canPosY) = (plX - (div canSize 2),plY - (div canSize 2)) 
       
       args <- getArgs
       
       scr <- SDL.setVideoMode screenwidth screenheight 32 [SDL.SWSurface]
       tilesData <- loadImage "./images/bigAlphaTiles.png"
       
       let !system = Sys screenwidth screenheight fpsm
           !current = Chunk Continent (canPosX,canPosY) land (canSize,canSize) nbrPts 0
           !player' = Player (plX,plY) maje Stop
           !world = (addVarious seed).addBorders $ World system scr tilesData current [] player'
       

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
                SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _) -> return ()
                --SDL.KeyUp (SDL.Keysym _ _ _) -> loop $ changeDir w' Stop
                SDL.NoEvent -> loop $ changeDir w' Stop
                _       -> loop w'
       
       loop world
