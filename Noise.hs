module Noise (perlin2D, noiseMat) where
import GHC.Int
import Data.Bits
import qualified Data.List as List
import System.Random
import qualified Data.Vector as Vec
--import Criterion.Main

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

----------------------------------------------------------------------------------------------------------------
{- Vectors -}

type Mat a = Vec.Vector a

fromMat :: [[a]] ->  Mat a
fromMat xs = Vec.fromList [Vec.fromList xs' | xs' <- xs]

(§) :: Mat a -> (Int, Int) -> a
v § (r, c) = (v Vec.! r) Vec.! c

printVec :: (Show a) => Mat a -> String
printVec v | (Vec.length $ Vec.tail v) == 0 = drop 9 $ (show $ Vec.head v)
           | otherwise = drop 9 $ (show $ Vec.head v) ++ '\n':printVec (Vec.tail v)

whnfElements :: Vec.Vector a -> Vec.Vector a
whnfElements v = Vec.foldl' (flip seq) () v `seq` v

vmap' :: (a -> b) -> Vec.Vector a -> Vec.Vector b
vmap' f = whnfElements . Vec.map f

matMap :: (a -> b) -> Mat a -> Mat b
matMap  f = (vmap' . vmap') f

--(oct,per,nbrPts,nbrSummit,nbrCol,seed)
noiseMat :: Int -> Double -> Int -> Int -> Int -> Int -> Mat Int
noiseMat oct per nbrPts nbrSummit nbrCol seed =
    let summits = (fI nbrPts / fI nbrSummit)
        p (i,j) = perlin2D (i,j) oct per seed        

        getVals xs 0 maxi mini = (xs, maxi, mini)
        getVals xs r maxi mini = let getRow xs 0 max' min' = (xs,max',min')
                                     getRow xs c max' min' = let v = p ((fI r)/summits,(fI c)/summits)
                                                                 max'' = max max' v
                                                                 min'' = min min' v
                                                             in getRow (v:xs) (c-1) max'' min''
                                     
                                     (rs,max'',min'') = getRow [] nbrPts maxi mini 
                                
                                 in getVals (rs:xs) (r-1) max'' min''

        (vals,maxi, mini) = getVals [] nbrPts 0.0 (p (0,0))

        toScale v = let v' = (fI nbrCol) * (v-mini)/(maxi-mini)                       
                    in round v'

    in (matMap toScale).fromMat $ vals


noiseMat2 :: Int -> Double -> Int -> Int -> Int -> Int -> Mat Int
noiseMat2 oct per nbrPts nbrSummit nbrCol seed =
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
        

{- Benchmarking -}
{-
main = defaultMain [
  bgroup "noiseMat" [ bench "1"  $ whnf (noiseMat 12 1 200 20 255) 12
                    , bench "2"  $ whnf (noiseMat2 12 1 200 20 255) 12
                    ]
  ]
-}

      
       