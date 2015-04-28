import Control.Monad
import Control.Monad.ST
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLP
import qualified Graphics.UI.SDL.Image as SDLI
import qualified Graphics.UI.SDL.Framerate as SDLF
import GHC.Word
import GHC.Int
import Data.Bits
import qualified Data.List as List
import System.Random
import System.Environment
import qualified Data.Vector as Vec
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Noise
import Data.Graph.Inductive
import Control.DeepSeq
import Tiles hiding (slow)

data Player = Player { plPos :: (Int,Int)
                     , plTiles :: Tile
                     , direct :: Direction
                     }

data Sys = Sys { width :: Int
               , height :: Int
               , fps :: SDLF.FPSManager
               }

data ChunkType = Islands | Continent | Mountains

data Chunk = Chunk { chType :: ChunkType
                   , chPos :: (Int,Int)
                   , chLand :: Mat [[Tile]]
                   , canvasSize :: (Int,Int)
                   , chunkSize :: Int
                   , chunkNbr :: Int
                   }

data World = World { sys :: Sys
                   , screen :: SDL.Surface
                   , tileset :: SDL.Surface
                   , chunk :: Chunk
                   , chunks :: [Chunk]
                   , player :: Player
                   }

data Direction = Lefty | Righty| Up | Down | Stop

{-# LANGUAGE BangPatterns #-}

{- Main -}

screenwidth = 1000
screenheight = 640
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
           land = go.putMageIn $ vmap' noise2Tile (noiseMat oct per nbrPts nbrSum nbrCol seed)
           
           --width/height of displayed canvas (in tiles)
           canSize = 19

           --Starting position in chunk
           (chPosX,chPosY) = let off = (div nbrPts 2) - (div canSize 2) in (off,off)
       
       args <- getArgs
       
       scr <- SDL.setVideoMode screenwidth screenheight 32 [SDL.SWSurface]
       tilesData <- loadImage "bigAlphaTiles.png"
       
       let system = Sys screenwidth screenheight fpsm
           current = Chunk Islands (chPosX,chPosY) land (canSize,canSize) nbrPts 0
           player' = Player (100,100) (Being Mage2) Stop
           world = World system scr tilesData current [] player'

       let loop w = 
            do let (t,s,fpsm) = (tileset w, screen w, fps.sys $ w)
                   (ch,(chX,chY)) = (chunk $ w, chPos.chunk $ w)
                   (wid,hei) = (width.sys $ w, height.sys $ w)
                   ch' = ch { chLand = go (chLand ch)}
                   --go v = addThings v [(2,3,Building Tower),(45,23,Building SmallCastle2),(100,80,Being Mage4)]
                       
               SDLP.filledPolygon s [(0,0),(fI wid,0),(fI wid,fI hei),(0,fI hei)] (getPixel 0 0 0)
               ch' <- tileList ch t s
               SDL.flip s
               
               let mov = movePlayer.addPlayer
                   w' = mov (w {chunk = ch'}) 
               
               event      <- SDL.pollEvent
               SDLF.delay fpsm
                       
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

--------------------------------------------------------------------------------------------------------
{- misc helper functions-}

osc :: Int -> Int -> Int -> [Int]
osc a b c = cycle $ [a,a+c..b] ++ [b,b-c..a+c]

slow :: Int -> [a] -> [a]
slow 0 xs = xs
slow n [] = []
slow n (x:xs) = go n x [] ++ (slow n xs) where go 0 _ xs = xs
                                               go n x xs = go (n-1) x (x:xs)

whnfList xs = List.foldl' (flip seq) () xs `seq` xs

fI = fromIntegral

changeDir :: World -> Direction -> World
changeDir w d =  w { player = (player w) {direct = d}}

-----------------------------------------------------------------------------------------------------
{- Vectors -}

type Mat a = Vec.Vector a

--printVec :: (Show a) => Mat a -> String
--printVec v | (Vec.length $ Vec.tail v) == 0 = drop 9 $ (show $ Vec.head v)
--           | otherwise = drop 9 $ (show $ Vec.head v) ++ '\n':printVec (Vec.tail v)

(§) :: Mat a -> (Int, Int) -> a
v § (r, c) = v Vec.! (r*nbrPts + c)

whnfElements :: Vec.Vector a -> Vec.Vector a
whnfElements v = Vec.foldl' (flip seq) () v `seq` v

vmap' :: (a -> b) -> Vec.Vector a -> Vec.Vector b
vmap' f = whnfElements . Vec.map f

vImap' :: (Int -> a -> b) -> Vec.Vector a -> Vec.Vector b
vImap' f = whnfElements.Vec.imap f

updates :: Mat a -> [(Int,Int,a)] -> Mat a
updates m xs = List.foldl' update m xs

update :: Mat a -> (Int,Int,a) -> Mat a
update m (i,j,v) = runST $ do m' <- Vec.thaw m
                              GM.write m' (i*nbrPts+j) v
                              Vec.freeze m'

updateTail ::  Mat [[Tile]] -> Mat [[Tile]]
updateTail m = runST $ do m' <- Vec.thaw (m)
                          let loop 0 = return ()
                              loop n = do (x:xs) <- GM.read m' n
                                          deepseq x (GM.write m' n xs)
                                          loop (n-1)
                          loop ((nbrPts*nbrPts - 1))
                          Vec.freeze m'

-------------------------------------------------------------------------------------------------
{- Graphics -}

type Point = (Double,Double)

surfaceSize :: SDL.Surface -> (Int,Int)
surfaceSize s = (SDL.surfaceGetWidth s, SDL.surfaceGetHeight s)

getPixel :: Word8 -> Word8 -> Word8 -> SDL.Pixel
getPixel r g b = SDL.Pixel $ (shiftL (fi r) 24 .|. shiftL (fi g) 16 .|. shiftL (fi b) 8 .|. (fi 255)) where 
  fi a = fromIntegral a

pixelsToScreen :: [Point] -> SDL.Surface -> SDL.Pixel -> [IO Bool]
pixelsToScreen xs s p = map (\(x,y) -> SDLP.pixel s (fI.round $ x) (fI.round $ y) p) xs

linesToSur :: [((Int,Int),(Int,Int))] -> SDL.Surface -> SDL.Pixel -> [IO Bool]
linesToSur xs s p = map (\((x1,y1),(x2,y2)) -> SDLP.line s (fI x1) (fI y1) (fI x2) (fI y2) p) xs

loadImage :: String -> IO SDL.Surface
loadImage filename = SDLI.load filename  >>= SDL.displayFormatAlpha

applySurface :: Int -> Int -> SDL.Surface -> SDL.Surface -> IO Bool
applySurface x y src dst = SDL.blitSurface src clip dst offset
 where offset = Just SDL.Rect { SDL.rectX = x, SDL.rectY = y, SDL.rectW = 0, SDL.rectH = 0 }
       clip = Just SDL.Rect { SDL.rectX = 0, SDL.rectY = 0, SDL.rectW = fst $ surfaceSize src, SDL.rectH = snd $ surfaceSize src }

---------------------------------------------------------------------------------------------------
{- Engine -}


{-- Chunk update --}

putMageIn :: Mat [[Tile]] -> Mat [[Tile]]
putMageIn m = let tileStack = head $ m § (100,100) --[Tile]
                  m1 = slow 20 [tileStack ++ [Being Mage1]]
                  m2 = slow 10 [tileStack ++ [Being Mage2]]
                  m3 = slow 10 [tileStack ++ [Being Mage3]]
                  m4 = slow 10 [tileStack ++ [Being Mage4]]
              in update m (100,100,cycle $ m1 ++ m2 ++ m3 ++ m4) 


addThings :: Mat [[Tile]] -> [(Int,Int,Tile)] -> Mat [[Tile]]
addThings m xs = runST $ do m' <- Vec.thaw m
                            let loop [] = return ()
                                loop ((i,j,t):xs) = do let ind = i * nbrPts + j
                                                       tileStack <- GM.read m' ind
                                                       let t' = [(head tileStack) ++ [t]] 
                                                       GM.write m' ind (cycle (deepseq t' t'))
                                                       loop xs
                            loop xs
                            Vec.freeze m'

addPlayer :: World -> World
addPlayer w = let p = player w
                  ((j,i),t) = (plPos p, plTiles p)
                  l = chLand.chunk $ w
                  ind = i * nbrPts + j
 
                  newl = runST $ do l' <- Vec.thaw l
                                    tileStack <- GM.read l' ind
                                    let t' = [(head tileStack) ++ [t]] 
                                    GM.write l' ind (cycle (deepseq t' t'))
                                    Vec.freeze l'
              in w {chunk = (chunk w){chLand = newl}}
                            



{-- Screen drawing --}

applyTile :: [Tile] -> (Int,Int) -> SDL.Surface -> SDL.Surface -> IO [Bool]
applyTile ts (x,y) src dest =
    let offset = Just SDL.Rect { SDL.rectX = x, SDL.rectY = y, SDL.rectW = 32, SDL.rectH = 32}
        tileRects = map getTile ts
    in sequence $ map (\tr -> SDL.blitSurface src tr dest offset) tileRects

applyTileMat :: Chunk -> SDL.Surface -> SDL.Surface -> IO Chunk
applyTileMat ch src dest = 
  let m = chLand $! ch
      (x,y) = chPos ch
      (canW,canH) = canvasSize ch in

  do sequence $ [ applyTile (head (m § (i,j))) (32*(j-x), 32*(i-y)) src dest | i <- [y..(y+canH)], j <- [x..(x+canW)]]
     let m' = updateTail (m) 
     return ch { chLand = m' }

tileList :: Chunk -> SDL.Surface -> SDL.Surface -> IO Chunk
tileList ch src dest = applyTileMat ch src dest >>= (\m' -> return m')


{-- Movments --}

moveCamera :: World -> World
moveCamera w  = let maxBound = (chunkSize.chunk $ w) - (uncurry max) (canvasSize.chunk $ w)
                    isInBounds (i,j) = (i >= 0 && i < maxBound) && (j >= 0 && j < maxBound)
                    (chX,chY) = (chPos.chunk $ w)
                    ch = chunk w
                    p = player w in
                 
                 case direct p of
                           Stop -> w
                           Lefty  -> if isInBounds (chX - 1, chY)
                                    then w {chunk = ch {chPos = (chX-1,chY)}}
                                    else w
                           Righty -> if isInBounds (chX + 1, chY)
                                    then w {chunk = ch {chPos = (chX+1,chY)}}
                                    else w
                           Up    -> if isInBounds (chX, chY - 1)
                                    then w {chunk = ch {chPos = (chX,chY - 1)}}
                                    else w
                           Down  -> if isInBounds (chX, chY + 1)
                                    then w {chunk = ch {chPos = (chX,chY + 1)}}
                                    else w

movePlayer :: World -> World
movePlayer w = let maxBound = nbrPts
                   isInBounds (i,j) = (i >= 0 && i < maxBound) && (j >= 0 && j < maxBound)
                   (pX,pY) = (plPos.player $ w)
                   p = player w in
               
               case direct p of
                           Stop -> w
                           Lefty  -> if isInBounds (pX - 1, pY)                                     
                                     then w {player = p {plPos = (pX - 1, pY)}} 
                                     else w
                           Righty -> if isInBounds (pX + 1, pY)
                                     then w {player = p {plPos = (pX+1,pY)}}
                                     else w
                           Up    -> if isInBounds (pX, pY - 1)
                                    then w {player = p {plPos = (pX,pY - 1)}}
                                    else w
                           Down  -> if isInBounds (pX, pY + 1)
                                    then w {player = p {plPos = (pX,pY + 1)}}
                                    else w
 
----------------------------------------------------------------------------------------------------
     