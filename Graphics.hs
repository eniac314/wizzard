module Graphics where
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLP
import qualified Graphics.UI.SDL.Image as SDLI
import Control.Monad
import Data.Bits
import GHC.Word
import GHC.Int
import Helper
import EngineTypes
import qualified Data.Vector as Vec((!))
import Vector
import Tiles hiding (slow)



type Point = (Int16,Int16)
type Width = Int
type Height = Int
type Pixel = (Word8,Word8,Word8,Word8)


surfaceSize :: SDL.Surface -> (Int,Int)
surfaceSize s = (SDL.surfaceGetWidth s, SDL.surfaceGetHeight s)

getPixel :: Word8 -> Word8 -> Word8 -> SDL.Pixel
getPixel r g b = SDL.Pixel $ (shiftL (fi r) 24 .|. shiftL (fi g) 16 .|. shiftL (fi b) 8 .|. (fi 255)) where 
  fi a = fromIntegral a

getAlphaPixel :: Word8 -> Word8 -> Word8 ->  Word8 -> SDL.Pixel
getAlphaPixel r g b a = SDL.Pixel $ (shiftL (fi r) 24 .|. shiftL (fi g) 16 .|. shiftL (fi b) 8 .|. (fi a)) where 
  fi n = fromIntegral n


pixelsToScreen :: [Point] -> SDL.Surface -> SDL.Pixel -> [IO Bool]
pixelsToScreen xs s p = map (\(x,y) -> SDLP.pixel s  x y p) xs

linesToSur :: [((Int,Int),(Int,Int))] -> SDL.Surface -> SDL.Pixel -> [IO Bool]
linesToSur xs s p = map (\((x1,y1),(x2,y2)) -> SDLP.line s (fromIntegral x1) (fromIntegral y1) (fromIntegral x2) (fromIntegral y2) p) xs

loadImage :: String -> IO SDL.Surface
loadImage filename = SDLI.load filename  >>= SDL.displayFormatAlpha

applySurface :: Int -> Int -> SDL.Surface -> SDL.Surface -> IO Bool
applySurface x y src dst = SDL.blitSurface src clip dst offset
 where offset = Just SDL.Rect { SDL.rectX = x, SDL.rectY = y, SDL.rectW = 0, SDL.rectH = 0 }
       clip = Just SDL.Rect { SDL.rectX = 0, SDL.rectY = 0, SDL.rectW = fst $ surfaceSize src, SDL.rectH = snd $ surfaceSize src }

drawBackground :: Width -> Height -> SDL.Surface -> IO Bool
drawBackground wid hei s = SDLP.filledPolygon s [(0,0),(fromIntegral wid,0),(fromIntegral wid,fromIntegral hei),(0,fromIntegral hei)] (getPixel 0 0 0)

drawAlphaPoly :: Point -> Width -> Height -> Pixel -> SDL.Surface -> IO Bool
drawAlphaPoly (x,y) wid hei (r,g,b,a) s = 
  SDLP.filledPolygon s [(x,y),(fromIntegral wid,y),(fromIntegral wid,fromIntegral hei),(x,fromIntegral hei)] (getAlphaPixel r g b a)

{-- Screen rendering --}

applyTile :: TileStack -> (Int,Int) -> SDL.Surface -> SDL.Surface -> IO [Bool]
applyTile ts (x,y) src dest =
    let offset = Just SDL.Rect { SDL.rectX = x, SDL.rectY = y, SDL.rectW = 32, SDL.rectH = 32}
        tileRects = map getTileCoord ts
    in sequence $ map (\tr -> SDL.blitSurface src tr dest offset) tileRects

applyTileMat :: World -> SDL.Surface -> SDL.Surface -> IO ()
applyTileMat w src dest = 
  let n = getChunkSize w
      m = getTiles w
      (x,y) = getCanvasPos w
      (canW,canH) = getCanvasSize w in

  do sequence $  [ applyTile (head (m § (n*i+j))) (32*(j-x), 32*(i-y)) src dest | i <- [y..(y+canH)], j <- [x..(x+canW)]]
     return () 

applyPlayer :: World -> SDL.Surface -> SDL.Surface -> IO [Bool]
applyPlayer w src dest =
  let t = head.getPlayerTiles $ w
      n = getChunkSize w
      (plX,plY) = getPlayerPos w
      (canX,canY) = getCanvasPos w
      l = getTiles w
      tileStack = head (l § (n*plY+plX))
      (x',y') = (plX - canX, plY - canY)
  in applyTile (tileStack ++ t) (32*x',32*y') src dest
