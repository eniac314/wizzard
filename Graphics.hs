module Graphics where
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLP
import qualified Graphics.UI.SDL.Image as SDLI
import Data.Bits
import GHC.Word
import GHC.Int



type Point = (Double,Double)


surfaceSize :: SDL.Surface -> (Int,Int)
surfaceSize s = (SDL.surfaceGetWidth s, SDL.surfaceGetHeight s)

getPixel :: Word8 -> Word8 -> Word8 -> SDL.Pixel
getPixel r g b = SDL.Pixel $ (shiftL (fi r) 24 .|. shiftL (fi g) 16 .|. shiftL (fi b) 8 .|. (fi 255)) where 
  fi a = fromIntegral a

pixelsToScreen :: [Point] -> SDL.Surface -> SDL.Pixel -> [IO Bool]
pixelsToScreen xs s p = map (\(x,y) -> SDLP.pixel s (fromIntegral.round $ x) (fromIntegral.round $ y) p) xs

linesToSur :: [((Int,Int),(Int,Int))] -> SDL.Surface -> SDL.Pixel -> [IO Bool]
linesToSur xs s p = map (\((x1,y1),(x2,y2)) -> SDLP.line s (fromIntegral x1) (fromIntegral y1) (fromIntegral x2) (fromIntegral y2) p) xs

loadImage :: String -> IO SDL.Surface
loadImage filename = SDLI.load filename  >>= SDL.displayFormatAlpha

applySurface :: Int -> Int -> SDL.Surface -> SDL.Surface -> IO Bool
applySurface x y src dst = SDL.blitSurface src clip dst offset
 where offset = Just SDL.Rect { SDL.rectX = x, SDL.rectY = y, SDL.rectW = 0, SDL.rectH = 0 }
       clip = Just SDL.Rect { SDL.rectX = 0, SDL.rectY = 0, SDL.rectW = fst $ surfaceSize src, SDL.rectH = snd $ surfaceSize src }

drawBackground :: Int -> Int -> SDL.Surface -> IO Bool
drawBackground wid hei s = SDLP.filledPolygon s [(0,0),(fromIntegral wid,0),(fromIntegral wid,fromIntegral hei),(0,fromIntegral hei)] (getPixel 0 0 0)
