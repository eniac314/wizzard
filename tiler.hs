import Control.Monad
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLP
import qualified Graphics.UI.SDL.Image as SDLI
import qualified Graphics.UI.SDL.Framerate as SDLF
import GHC.Word
import GHC.Int
import Data.Bits
import qualified Data.Set as Set
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

data Tile = Water WaterType 
          | Land LandType deriving Show

data Sys = Sys { width :: Int
               , height :: Int
               , fps :: SDLF.FPSManager
               }

data World = World { sys :: Sys
                   , screen :: SDL.Surface
                   , tiles :: SDL.Surface
                   , chunk :: Mat [Tile]
                   , chunks :: [Mat [Tile]]
                   }

--------------------------------------------------------------------------------------------------------
{- misc helper functions-}

osc :: Int -> Int -> Int -> [Int]
osc a b c = cycle $ [a,a+c..b] ++ [b,b-c..a+c]

slow :: Int -> [a] -> [a]
slow 0 xs = xs
slow n [] = []
slow n (x:xs) = go n x [] ++ (slow n xs) where go 0 _ xs = xs
                                               go n x xs = go (n-1) x (x:xs)

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

getTile :: Tile -> Maybe SDL.Rect
getTile t = 
	let (i,j) = case t of Water Shallow -> (0,3)
	                      Water Deep1 -> (7,31)
	                      Water Deep2 -> (0,2)
	                      Land Swamp -> (0,4)
	                      Land GrassLand -> (0,5)
	                      Land HighMountain1 -> (0,12)
	                      Land HighMountain2 -> (0,13)
	                      Land LowMountain -> (0,10)
	                      Land SmallTrees -> (0,8)
	                      Land Forest -> (0,9)
	                      _              -> (0,7)	                      
	in Just SDL.Rect { SDL.rectX = j * 32, SDL.rectY = i * 32 , SDL.rectW = 32, SDL.rectH = 32}

noise2Tile n | n == 0 = cycle $ slow 4 [Water Deep2,Water Shallow]
             | n == 1 = cycle $ slow 4 [Water Deep2,Water Shallow]
             | n == 2 = cycle [Land Swamp]
             | n == 3 = cycle [Land GrassLand]
             | n == 4 = cycle [Land SmallTrees]
             | n == 5 = cycle [Land Forest]
             | n == 6 = cycle [Land LowMountain]
             | n == 7 = cycle [Land HighMountain1]


applyTile :: Tile -> (Int,Int) -> SDL.Surface -> SDL.Surface -> IO Bool
applyTile tile (x,y) src dest =
    let offset = Just SDL.Rect { SDL.rectX = x, SDL.rectY = y, SDL.rectW = 32, SDL.rectH = 32}
        tileRect = getTile tile
    in SDL.blitSurface src tileRect dest offset

applyTileMat :: Mat [Tile] -> SDL.Surface -> SDL.Surface -> IO (Mat [Tile])
applyTileMat m src dest = 
  let wid = Vec.length (m Vec.! 0) - 1
      hei = (Vec.length m) - 1 in
  do sequence $ [ applyTile (head (m § (i,j))) (32*j, 32*i) src dest | i <- [0..hei], j <- [0..wid]]
     return $ fromMat [[(tail (m § (i,j))) | j <- [0..wid]] | i <- [0..hei]]

tileList :: Mat [Tile] -> SDL.Surface -> SDL.Surface -> IO (Mat [Tile])
tileList m src dest = applyTileMat m src dest >>= (\m' -> return m')

-----------------------------------------------------------------------------------------------------
{- Vectors -}

type Mat a = Vec.Vector (Vec.Vector a)

fromMat :: [[a]] ->  Mat a
fromMat xs = Vec.fromList [Vec.fromList xs' | xs' <- xs]

(§) :: Mat a -> (Int, Int) -> a
v § (r, c) = (v Vec.! r) Vec.! c

printVec :: (Show a) => Mat a -> String
printVec v | (Vec.length $ Vec.tail v) == 0 = drop 9 $ (show $ Vec.head v)
           | otherwise = drop 9 $ (show $ Vec.head v) ++ '\n':printVec (Vec.tail v)

matMap :: (a -> b) -> Mat a -> Mat b
matMap f m = let wid = Vec.length (m Vec.! 0) - 1
                 hei = (Vec.length m) - 1

                 vals = [[f (m § (i,j)) | j <- [0..wid]] | i <- [0..hei]]
                 
             in fromMat vals
                    


----------------------------------------------------------------------------------------------------
{- Main -}



screenwidth = 1000
screenheight = 600

main = SDL.withInit [SDL.InitEverything] $ do
       
       fpsm <- SDLF.new
       SDLF.init fpsm
       
       gen <- getStdGen
       
       let (seed,_) = random gen
           land = matMap noise2Tile (noiseMat 18 0.2 30 25 7 seed)
       
       args <- getArgs
       
       scr <- SDL.setVideoMode screenwidth screenheight 32 [SDL.SWSurface]
       tilesData <- loadImage "bigAlphaTiles.png"
       
       let system = Sys screenwidth screenheight fpsm
           world = World system scr tilesData land []

       let loop w = do let (ch,t,s,fpsm) = (chunk w, tiles w, screen w, fps.sys $ w)
                           (wid,hei) = (width.sys $ w, height.sys $ w) 
                       
                       SDLP.filledPolygon s [(0,0),(fI wid,0),(fI wid,fI hei),(0,fI hei)] (getPixel 0 0 0)
                       ch' <- tileList ch t s
                       SDL.flip s
                       
       	               event      <- SDL.pollEvent
                       SDLF.delay fpsm
                       
                       case event of
                            SDL.Quit -> return ()
                            SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _) -> return ()
                            SDL.NoEvent -> loop w {chunk = ch'} 
                            _       -> loop w
       loop world