module Noise (perlin2D, noiseMat) where
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLP
import GHC.Word
import GHC.Int
import Data.Bits
import qualified Data.Set as Set
import qualified Data.List as List
import System.Random
import Data.Fixed (mod')
import qualified Data.Vector as Vec

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

flatMap f [] = []
flatMap f (x:xs) = (f x) ++ flatMap f xs

randList :: Int -> [Int]
randList n = let gen = mkStdGen n in randomRs (0,1000) gen

-- deterministic noise function, output in [0,1]
noise2d :: (Int, Int) -> Double
noise2d (x, y) = 
    let m = x + y * 57
        n = xor (shiftR m 13) m
        j = (n * (n * n * 15731 + 789221) + 1376312589) .&. 0x7fffffff
    in  (2.0 - (fromIntegral j / 1073741824.0))/2

interpolate :: Double -> Double -> Double -> Double
interpolate a b x = let ft = x * pi
                        f = (1 - cos(ft)) * 0.5
                    in a * (1 - f) + b * f

smoothNoise2d :: (Int, Int) -> Double
smoothNoise2d (x, y) =
  let corners = (noise2d(x-1, y-1) + noise2d(x+1, y-1) + noise2d(x-1, y+1) + noise2d(x+1, y+1)) / 16
      sides = (noise2d(x-1, y) + noise2d(x+1, y) + noise2d(x, y-1) + noise2d(x, y+1)) /  8
      center =  noise2d(x, y) / 4
  in corners + sides + center

--smoothNoise2d = noise2d

perlin2D :: (Double,Double) -> Int -> Double -> Int -> Double
perlin2D (x,y) n p s = let rands = randList s

                           interNoise (x,y) = 
                              let intX = floor x
                                  fracX = x - (fI intX)

                                  intY = floor y
                                  fracY = y - (fI intY)

                                  v1 = smoothNoise2d (intX, intY)
                                  v2 = smoothNoise2d (intX + 1, intY)
                                  v3 = smoothNoise2d (intX, intY + 1)
                                  v4 = smoothNoise2d (intX + 1, intY + 1)

                                  i1 = interpolate v1 v2 fracX
                                  i2 = interpolate v3 v4 fracX

                              in interpolate i1 i2 fracY

                           go 0 t (r:rs) = t + interNoise (((fI r) + x), ((fI r) + y))
                           go n t (r:rs) = go (n-1) (t + interNoise (((fI r) + x)*2^n, ((fI r) + y)*2^n) * p^n) rs
                       in go (n-1) 0 rands

------------------------------------------------------------------------------------------------------------------------
{- Graphics -}

type Point = (Double,Double)
type Pxl = (Int16,Int16,SDL.Pixel)

getPixel ::(Word8, Word8, Word8) -> SDL.Pixel
getPixel (r, g, b) = SDL.Pixel $ (shiftL (fi r) 24 .|. shiftL (fi g) 16 .|. shiftL (fi b) 8 .|. (fi 255)) where 
	fi a = fromIntegral a

pixelsToScreen :: [Point] -> SDL.Surface -> SDL.Pixel -> [IO Bool]
pixelsToScreen xs s p = map (\(x,y) -> SDLP.pixel s (fromIntegral.round $ x) (fromIntegral.round $ y) p) xs

zoomPix :: Int16 -> Int16 -> [Pxl] -> [Pxl]
zoomPix h v ps = let helper (x,y,p) = [((h*x)+i,(v*y)+j,p) | i <- [0..(h-1)], j <- [0..(v-1)]]
              in flatMap helper ps

zoomRect :: Int16 -> Int16 -> [Pxl] -> [(SDL.Rect,SDL.Pixel)]
zoomRect h v ps = let ps1 = map (\(x,y,p) -> (x*h,y*v,p)) ps
               in map (\(x,y,p) -> (SDL.Rect {SDL.rectX = fI x, SDL.rectY = fI y, SDL.rectW = fI h,SDL.rectH = fI v},p)) ps1

zoomPoly :: Int16 -> Int16 -> [Pxl] -> [([(Int16,Int16)],SDL.Pixel)]
zoomPoly h v ps = let helper (x,y,p) = ([(x*h,y*v),(x*h+(h),y*v),(x*h+(h),y*v+(v)),(x*h,y*v+(v))],p)
               in map helper ps


--http://www.rapidtables.com/convert/color/hsv-to-rgb.htm
hsvToRgb :: (Double,Double,Double) -> (Word8,Word8,Word8)
hsvToRgb (h,s,v) =  let c = v * s
                        x = c * (1 - (abs $ (mod' (h / 60) 2) - 1)) --X = C × (1 - |(H / 60º) mod 2 - 1|)
                        m = v - c
                        (r',g',b') = getRgb' c x

                    in (round.(255 *) $ r'+ m, round.(255 *) $ g'+ m, round.(255 *) $ b'+ m)
                    
                    where getRgb' c x | 0 <= h && h < 60 = (c,x,0)
                                      | 60 <= h && h < 120 = (x,c,0)
                                      | 120 <= h && h < 180 = (0,c,x)
                                      | 180 <= h && h < 240 = (0,x,c)
                                      | 240 <= h && h < 300 = (x,0,c)
                                      | 300 <= h && h < 360 = (c,0,x)


------------------------------------------------------------------------------------------------------------------------


{- Vectors -}

type Mat a = Vec.Vector (Vec.Vector a)

fromMat :: [[a]] ->  Mat a
fromMat xs = Vec.fromList [Vec.fromList xs' | xs' <- xs]

(§) :: Mat a -> (Int, Int) -> a
v § (r, c) = (v Vec.! r) Vec.! c

printVec :: (Show a) => Mat a -> String
printVec v | (Vec.length $ Vec.tail v) == 0 = drop 9 $ (show $ Vec.head v)
           | otherwise = drop 9 $ (show $ Vec.head v) ++ '\n':printVec (Vec.tail v)

--(oct,per,nbrPts,nbrSummit,nbrCol,seed)
noiseMat :: Int -> Double -> Int -> Int -> Int -> Int -> Mat Int
noiseMat oct per nbrPts nbrSummit nbrCol seed =
    let summits = (fI nbrPts / fI nbrSummit)
        p (i,j) = perlin2D (i,j) oct per seed        
        
        vals = [(p ((fI i)/summits,(fI j)/summits)) | i <- [0..nbrPts-1], j <- [0..nbrPts-1]]

        (mini,maxi) = (List.minimum vals, List.maximum vals)

        toScale v = let v' = (fI nbrCol) * (v-mini)/(maxi-mini)                       
                    in round v'
        
        scaledVals = map (\n -> (toScale n)) vals 


        helper [] = []
        helper xs = let (row,xs') = splitAt nbrPts xs
                    in row:helper xs'

    in fromMat $ helper scaledVals

        





      
       