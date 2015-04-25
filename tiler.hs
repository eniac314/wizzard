import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLP
import qualified Graphics.UI.SDL.Image as SDLI
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

noise2Tile n | n == 0 = Water Deep2
             | n == 1 = Water Shallow
             | n == 2 = Land Swamp
             | n == 3 = Land GrassLand
             | n == 4 = Land SmallTrees
             | n == 5 = Land Forest
             | n == 6 = Land LowMountain
             | n == 7 = Land HighMountain1


applyTile :: Tile -> (Int,Int) -> SDL.Surface -> SDL.Surface -> IO Bool
applyTile tile (x,y) src dest =
    let offset = Just SDL.Rect { SDL.rectX = x, SDL.rectY = y, SDL.rectW = 32, SDL.rectH = 32}
        tileRect = getTile tile
    in SDL.blitSurface src tileRect dest offset

applyTileMat :: Mat Tile -> SDL.Surface -> SDL.Surface -> IO [Bool]
applyTileMat m src dest = 
	let wid = Vec.length (m Vec.! 0) - 1
	    hei = (Vec.length m) - 1 in
	sequence $ [ applyTile (m § (i,j)) (32*j, 32*i) src dest | i <- [0..hei], j <- [0..wid]]

tileList :: [Mat Tile] -> SDL.Surface -> SDL.Surface -> IO [Mat Tile]
tileList (t:ts) src dest = applyTileMat t src dest >> return ts

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







width = 1368
height = 960

main = SDL.withInit [SDL.InitEverything] $ do
       gen <- getStdGen
       
       let (seed,_) = random gen
           land = matMap noise2Tile (noiseMat 18 0.2 200 150 7 seed)
       
       args <- getArgs
       
       screen <- SDL.setVideoMode width height 32 [SDL.SWSurface]
       tiles <- loadImage "bigAlphaTiles.png"
       
       let landList = cycle [land]
           world = (landList, tiles, screen)

       --putStrLn $ (printVec land)
       

       let loop w = do SDL.delay 10
                       let (ls,t,s) = w
                       SDLP.filledPolygon screen [(0,0),(1368,0),(1368,960),(0,960)] (getPixel 0 0 0)
                       ls' <- tileList ls t s
                       SDL.flip s
       	               event      <- SDL.pollEvent
                       case event of
                            SDL.Quit -> return ()
                            SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _) -> return ()
                            SDL.NoEvent -> loop (ls',t,s) 
                            _       -> loop w
       loop world