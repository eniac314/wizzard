import Control.Monad
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
import Noise
import Data.Graph.Inductive



fI = fromIntegral

type Offset = (Int,Int)

data WaterType = Shallow
               | Deep1
               | Deep2
               | UpLeftDiag
               | UpRightDiag
               | DownLeftDiag
               | DownRightDiag deriving Show

data LandType = Swamp
              | GrassLand
              | Bushes
              | Thicket
              | SmallTrees
              | Forest
              | LowMountain 
              | HighMountain1
              | HighMountain2
              | Rocks1
              | Rocks2 
              | Field deriving Show

data BeingType  = Mage1
                | Mage2
                | Mage3
                | Mage4 deriving Show

data BuildingType = Tower
                  | SmallCastle1
                  | SmallCastle2 deriving Show

data Tile = Water WaterType 
          | Land LandType
          | Being BeingType
          | Building BuildingType deriving Show


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
                   }

data World = World { sys :: Sys
                   , screen :: SDL.Surface
                   , tiles :: SDL.Surface
                   , chunk :: Chunk
                   , chunks :: [Chunk]
                   , direct :: Direction
                   }

data Direction = Lefty | Righty| Up | Down | Stop

{-# LANGUAGE BangPatterns #-}

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

-----------------------------------------------------------------------------------------------------
{- Vectors -}

type Mat a = Vec.Vector a

--printVec :: (Show a) => Mat a -> String
--printVec v | (Vec.length $ Vec.tail v) == 0 = drop 9 $ (show $ Vec.head v)
--           | otherwise = drop 9 $ (show $ Vec.head v) ++ '\n':printVec (Vec.tail v)


whnfElements :: Vec.Vector a -> Vec.Vector a
whnfElements v = Vec.foldl' (flip seq) () v `seq` v

vmap' :: (a -> b) -> Vec.Vector a -> Vec.Vector b
vmap' f = whnfElements . Vec.map f

vImap' :: (Int -> a -> b) -> Vec.Vector a -> Vec.Vector b
vImap' f = whnfElements.Vec.imap f

--matMap :: (a -> b) -> Mat a -> Mat b
--matMap f = (vmap'.vmap') f

update :: Mat a -> (Int,Int,a) -> Mat a
update m (i,j,v) = vImap' (\k v' -> if k == i
                                    then vImap' (\l v'' -> if l == j then v else v'') v'
                                    else v') m
update m (i,j,v) = vImap' (k v')

updates :: Mat a -> [(Int,Int,a)] -> Mat a
updates m xs = List.foldl' update m xs 

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


getTile :: Tile -> Maybe SDL.Rect
getTile t = 
  let (i,j) = case t of Water Shallow -> (0,3)
                        Water Deep1 -> (0,1)
                        Water Deep2 -> (0,2)
                        Land Swamp -> (0,4)
                        Land GrassLand -> (0,5)
                        Land HighMountain1 -> (0,12)
                        Land HighMountain2 -> (0,13)
                        Land LowMountain -> (0,10)
                        Land SmallTrees -> (0,8)
                        Land Forest -> (0,9)
                        Being Mage1 ->(10,0)
                        Being Mage2 ->(10,1)
                        Being Mage3 ->(10,2)
                        Being Mage4 ->(10,3)
                        Building SmallCastle1 -> (0,20)
                        Building SmallCastle2 -> (0,21)
                        Building Tower -> (0,27)
                        _              -> (0,7)	                      
	in Just SDL.Rect { SDL.rectX = j * 32, SDL.rectY = i * 32 , SDL.rectW = 32, SDL.rectH = 32}

noise2Tile :: Int -> [[Tile]]
noise2Tile n | n == 0 = cycle $ slow 20 [[Water Deep2],[Water Deep1]]
             | n == 1 = cycle $ slow 20 [[Water Deep2],[Water Deep1]]
             | n == 2 = cycle [[Land Swamp]]
             | n == 3 = cycle [[Land GrassLand]]
             | n == 4 = cycle [[Land SmallTrees]]
             | n == 5 = cycle [[Land Forest]]
             | n == 6 = cycle [[Land LowMountain]]
             | n == 7 = cycle [[Land HighMountain1]]

putMageIn :: Mat [[Tile]] -> Mat [[Tile]]
putMageIn m = let tileStack = head $ m § (100,100) --[Tile]
                  m1 = slow 20 [tileStack ++ [Being Mage1]]
                  m2 = slow 10 [tileStack ++ [Being Mage2]]
                  m3 = slow 10 [tileStack ++ [Being Mage3]]
                  m4 = slow 10 [tileStack ++ [Being Mage4]]
              in update m (100,100,cycle $ m1 ++ m2 ++ m3 ++ m4) 

addThings :: Mat [[Tile]] -> [(Int,Int,Tile)] -> Mat [[Tile]]
addThings m xs = let helper (i,j,t) = let tileStack = head $ m § (i,j)
                                      in (i, j, cycle.whnfList $ [tileStack ++ [t]]) 
                     xs' = map helper xs
                 in updates m xs'              

              
applyTile :: [Tile] -> (Int,Int) -> SDL.Surface -> SDL.Surface -> IO [Bool]
applyTile ts (x,y) src dest =
    let offset = Just SDL.Rect { SDL.rectX = x, SDL.rectY = y, SDL.rectW = 32, SDL.rectH = 32}
        tileRects = map getTile ts
    in sequence $ map (\tr -> SDL.blitSurface src tr dest offset) tileRects

applyTileMat :: Chunk -> SDL.Surface -> SDL.Surface -> IO Chunk
applyTileMat ch src dest = 
  let m = chLand $! ch
      (x,y) = chPos ch
      wid = Vec.length (m Vec.! 0) - 1
      hei = (Vec.length m) - 1
      (canW,canH) = canvasSize ch in

  do sequence $ [ applyTile (head (m § (i,j))) (32*(j-x), 32*(i-y)) src dest | i <- [y..(y+canH)], j <- [x..(x+canW)]]
     let m' = (matMap tail) $ m
     return ch { chLand = m' }

tileList :: Chunk -> SDL.Surface -> SDL.Surface -> IO Chunk
tileList ch src dest = applyTileMat ch src dest >>= (\m' -> return m')

moveCamera :: World -> World
moveCamera w  = let maxBound = (chunkSize.chunk $ w) - (uncurry max) (canvasSize.chunk $ w)
                    isInBounds (i,j) = (i >= 0 && i < maxBound) && (j >= 0 && j < maxBound)
                    (chX,chY) = (chPos.chunk $ w)
                    ch = chunk w in
                 
                 case direct w of
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



----------------------------------------------------------------------------------------------------
{- Main -}



screenwidth = 1000
screenheight = 608

main = SDL.withInit [SDL.InitEverything] $ do
       
       fpsm <- SDLF.new
       SDLF.init fpsm
       
       gen <- getStdGen
       
       let (oct,per,nbrPts,nbrSum,nbrCol) = (18, 0.2, 200, 25, 7)
           (seed,_) = random gen
           --(seed,_) = random.mkStdGen $ 12
           
           --initial tile vector
           land = putMageIn $ matMap noise2Tile (noiseMat oct per nbrPts nbrSum nbrCol seed)
           
           --width/height of displayed canvas (in tiles)
           canSize = 19

           --Starting position in chunk
           (chPosX,chPosY) = let off = (div nbrPts 2) - (div canSize 2) in (off,off)
       
       args <- getArgs
       
       scr <- SDL.setVideoMode screenwidth screenheight 32 [SDL.SWSurface]
       tilesData <- loadImage "bigAlphaTiles.png"
       
       let system = Sys screenwidth screenheight fpsm
           current = Chunk Islands (chPosX,chPosY) land (canSize,canSize) nbrPts
           world = World system scr tilesData current [] Stop

       let loop w = 
            do let (t,s,fpsm) = (tiles w, screen w, fps.sys $ w)
                   (ch,(chX,chY)) = (chunk $ w, chPos.chunk $ w)
                   (wid,hei) = (width.sys $ w, height.sys $ w)
                   ch' = ch { chLand = go (chLand ch)}
                   go v = addThings v [(2,3,Building Tower),(45,23,Building SmallCastle2),(100,80,Being Mage4)]
                       
               SDLP.filledPolygon s [(0,0),(fI wid,0),(fI wid,fI hei),(0,fI hei)] (getPixel 0 0 0)
               ch'' <- tileList ch' t s
               SDL.flip s
               let w' = moveCamera (w {chunk = ch''})        

       	       event      <- SDL.pollEvent
               SDLF.delay fpsm
                       
               case event of
                SDL.Quit -> return ()
                SDL.KeyDown (SDL.Keysym SDL.SDLK_LEFT _ _) -> loop $  w' {direct = Lefty}
                SDL.KeyDown (SDL.Keysym SDL.SDLK_RIGHT _ _) -> loop $ w' {direct = Righty}
                SDL.KeyDown (SDL.Keysym SDL.SDLK_UP _ _) -> loop $ w' {direct = Up}
                SDL.KeyDown (SDL.Keysym SDL.SDLK_DOWN _ _) -> loop $ w' {direct = Down}
                SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _) -> return ()
                SDL.KeyUp (SDL.Keysym _ _ _) -> loop $ (w {direct = Stop})
                SDL.NoEvent -> loop w'
                _       -> loop w'
       
       loop world