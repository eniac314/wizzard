import Control.Monad
import qualified Data.List as List
import System.Random
import System.Environment
import qualified Data.Vector as Vec
import qualified Data.Text as Text
import SDL
import Linear
import Linear.Affine
import Noise
import Tiles hiding (slow)
import Helper
import EngineTypes
import Vector
import Graphics 
import Engine
import WorldUpdates
import Control.DeepSeq
import Control.Concurrent
import Control.DeepSeq

--import Euterpea

{-# LANGUAGE BangPatterns #-}

--t251 :: Music Pitch
--t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
--           gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
--           cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
--       in dMinor :+: gMajor :+: cMajor

{- Main -}

screenwidth = 672 :: Int
screenheight = 672 :: Int
nbrPts = 200
chunkType = Continent

fps = 30
ticksPerFrame = floor $ 1000 / fps

timeDelay start stop =
 let res = ticksPerFrame - (stop - start)
 in if (res > 0 && res < 100 ) then res else 0

initWindow = defaultWindow {windowInitialSize = V2 (fi screenwidth) (fi screenheight)}

vsyncRendererConfig = 
  RendererConfig
   { SDL.rendererType = SDL.AcceleratedVSyncRenderer
   , SDL.rendererTargetTexture = False
   }

main =
 do initializeAll

    HintRenderScaleQuality $= ScaleLinear
    renderQuality <- get HintRenderScaleQuality
    when (renderQuality /= ScaleLinear) $
     putStrLn "Warning: Linear texture filtering not enabled!"
    
    window <- createWindow (Text.pack "Wizzard 0.1") initWindow
    --scr <- createRenderer window (-1) vsyncRendererConfig
    scr <- createRenderer window (-1) defaultRenderer
    rendererDrawColor scr $= V4 0 0 0 0
       
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
       
    tilesData <- loadTexture scr "./images/bigAlphaTiles.gif"
       
    fmv <- newEmptyMVar
    putMVar fmv False
    bmv <- newEmptyMVar

    let !system = Sys screenwidth screenheight seed fmv bmv
        !current = Chunk chunkType (canPosX,canPosY) land (canSize,canSize) nbrPts 0
        !player' = Avatar (plX,plY) maje Stop
        !world = (addVarious seed).addBorders $ World system scr tilesData current [] player'
       

    --forkIO $ threadedChunk world bmv fmv

    let loop w = 
         do let (t,s) = (tileset w, screen w)
                (ch,(canX,canY)) = (chunk $ w, getCanvasPos $ w)
                (wid,hei) = (width.sys $ w, height.sys $ w)
                (fmv,bmv) = (flagMVar.sys $ w, boxMVar.sys $ w)
            
            start <- ticks

            --flag <- readMVar fmv
            --if flag then putStrLn "Done!" else putStrLn "not Yet!"
            --content <- tryTakeMVar bmv
            --case content of
            --       Nothing -> putStrLn "Empty" -- >> return w
            --       Just c  -> putStrLn "Good!!!!!!!!!!!!!!!!!!!!!!" -- >> (return $ w {chunk = ((chunk w){chLand = c})})

            {- Rendering -}       
            clear s
            applyTileMat w
            applyPlayer w
            present s
            

            {- World Update -}
            let nextFrames = updateTail.updatePlayerTiles 
            let w' = moveCamera.movePlayer.nextFrames $ w
               
            event <- pollEvent
            
            {- Fps cap -}
            stop <-ticks
            let del = (timeDelay start stop)
            unless (del == 0) (delay del)
            

            --printPlayerData w'
                       
            case event of
             Nothing -> loop $ changeDir w' Stop
             Just e -> 
              case eventPayload e of 
                QuitEvent -> return ()
                KeyboardEvent (KeyboardEventData _ _ _ (Keysym _ kc _)) -> 
                 case kc of KeycodeLeft -> loop $  changeDir w' Lefty
                            KeycodeRight -> loop $  changeDir w' Righty
                            KeycodeUp -> loop $  changeDir w' Up
                            KeycodeDown -> loop $  changeDir w' Down
                            KeycodeEscape -> return ()
                            _ -> loop w'
                _ -> loop w'            
       
    loop world
    destroyRenderer scr
    destroyWindow window
    quit


threadedChunk :: World -> MVar ([Mat [TileStack]])-> MVar Bool-> IO ()
threadedChunk w res flag = do let s = randSeed . sys $ w
                                  nbr = chunkSize . chunk $ w
                                  !newChunk1 = makeLand Continent nbr s
                                  !newChunk2 = makeLand Continent nbr (s+5)
                                  !newChunk3 = makeLand Continent nbr (s+3)
                                  !newChunk4 = makeLand Continent nbr (s+2)
                              --delay 5000
                              --seq newChunk (return ()) 
                              putMVar res [newChunk1,newChunk2,newChunk3,newChunk4]
                              putMVar flag True