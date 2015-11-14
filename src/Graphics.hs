module Graphics where
import SDL
import Linear
import Linear.Affine
import qualified Data.Text as Text
import Foreign.C.Types
import GHC.Word
import qualified SDL.Image

import Helper
import EngineTypes
import qualified Data.Vector as Vec((!))
import Vector
import Tiles hiding (slow)



type Color = (Word8,Word8,Word8,Word8)
type Pnt = (CInt,CInt)
type Width = CInt
type Height = CInt

textureSize :: Texture -> IO (CInt,CInt)
textureSize t = do TextureInfo _ _ w h <- queryTexture t
                   return (w,h)

pixelsToScreen :: [Pnt] -> Renderer -> Color -> IO [()]
pixelsToScreen ps r c = sequence $ map (dPoint r c) ps

dPoint :: Renderer -> Color -> Pnt -> IO ()
dPoint renderer (r,g,b,a) (x,y) = 
  do rendererDrawColor renderer $= V4 r g b a
     drawPoint renderer (P $ V2 x y) 


dLines :: Renderer -> Color -> [(Pnt,Pnt)] -> IO [()]
dLines r c ls = sequence $ map (\(s,d) -> dLine r c s d) ls

dLine :: Renderer -> Color -> Pnt -> Pnt -> IO ()
dLine renderer (r,g,b,a) (x1,y1) (x2,y2) = 
  do rendererDrawColor renderer $= V4 r g b a
     drawLine renderer (P $ V2 x1 y1) (P $ V2 x2 y2)

loadOptBMP :: Window -> String -> IO Surface
loadOptBMP window path = 
  do screenSurface <- getWindowSurface window
     loadedSurface <- loadBMP path
     desiredFormat <- surfaceFormat screenSurface
     convertSurface loadedSurface desiredFormat <* freeSurface loadedSurface


loadTexture :: Renderer -> String -> IO Texture
loadTexture r path = 
  do loadedSurface <- SDL.Image.load path
     texture <- createTextureFromSurface r loadedSurface
     freeSurface loadedSurface
     return texture


loadBMPTexture :: Renderer -> String -> IO Texture
loadBMPTexture r path = 
  do loadedSurface <- loadBMP path
     texture <- createTextureFromSurface r loadedSurface
     freeSurface loadedSurface
     return texture

loadBMPColorKeyTexture :: Renderer -> String -> Color -> IO Texture
loadBMPColorKeyTexture renderer path (r,g,b,a) = 
  do loadedSurface <- loadBMP path
     let key = V4 r g b a
     SDL.surfaceColorKey loadedSurface $= Just key
     texture <- createTextureFromSurface renderer loadedSurface
     freeSurface loadedSurface
     return texture

loadColorKeyTexture :: Renderer -> String -> Color -> IO Texture
loadColorKeyTexture renderer path (r,g,b,a) = 
  do loadedSurface <- SDL.Image.load path
     let key = V4 r g b a
     SDL.surfaceColorKey loadedSurface $= Just key
     texture <- createTextureFromSurface renderer loadedSurface
     freeSurface loadedSurface
     return texture

renderTexture :: Renderer -> Texture -> Pnt -> IO ()
renderTexture r t (x, y) = 
  do TextureInfo _ _ w h <- queryTexture t
     copy r t Nothing (Just $ Rectangle (P $ V2 x y) (V2 w h))

renderSprite :: Renderer -> Texture -> Pnt -> Pnt -> Width -> Height -> IO ()
renderSprite r t (xs, ys) (xd, yd) w h = 
  do let source = Just $ Rectangle (P $ V2 xs ys) (V2 w h)
         destination = Just $ Rectangle (P $ V2 xd yd) (V2 w h)
     copy r t source destination 


dFillRect :: Renderer -> Color -> Pnt -> Width -> Height -> IO ()
dFillRect renderer (r,g,b,a) (x,y) w h =
  do rendererDrawColor renderer $= V4 r g b a
     fillRect renderer (Just $ Rectangle (P $ V2 x y) (V2 w h))

applyTile :: TileStack -> Pnt -> Texture -> Renderer -> IO [()]
applyTile ts (x,y) src r =
    let tileRects = map getTileCoord ts
    in sequence $ map (\tr -> renderSprite r src tr (x,y) 32 32) tileRects


applyTileMat :: World -> IO ()
applyTileMat w = 
  let src = tileset w
      r = screen w
      n = getChunkSize w
      m = getTiles w
      (x,y) = getCanvasPos w
      (canW,canH) = getCanvasSize w in

  do sequence $  [ applyTile (head (m § (n*i+j))) (fi $ 32*(j-x),fi $ 32*(i-y)) src r | i <- [y..(y+canH)], j <- [x..(x+canW)]]
     return () 

applyPlayer :: World -> IO [()]
applyPlayer w  =
  let src = tileset w
      dest = screen w
      t = head.getPlayerTiles $ w
      n = getChunkSize w
      (plX,plY) = getPlayerPos w
      (canX,canY) = getCanvasPos w
      l = getTiles w
      tileStack = head (l § (n*plY+plX))
      (x',y') = (plX - canX, plY - canY)
  in applyTile (tileStack ++ t) (fi $ 32*x',fi $ 32*y') src dest

fi = fromIntegral