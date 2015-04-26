import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Primitives as SDLP
import GHC.Word
import GHC.Int
import Data.Bits
import qualified Data.Set as Set
import qualified Data.List as List
import System.Random
import System.Environment
import Data.Fixed (mod')

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

flatMap f [] = []
flatMap f (x:xs) = (f x) ++ flatMap f xs

randList :: Int -> [Int]
randList n = let gen = mkStdGen n in randomRs (0,1000) gen

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

{- Main -}

--compile with: ghc -O2 noise.hs -with-rtsopts= "-K100000k"


main = SDL.withInit [SDL.InitEverything] $ do
       args <- getArgs
       let (oct,per,h,v,nbrPts,nbrSummit,nbrCol,col) = 
       	     case args of (a:b:c:d:e:f:g:h:[]) -> (read a,read b,read c,read d,read e,read f, read g, read h)
       	                  _                -> (8,1,3,3,200,80,255,False)  
           
           width = nbrPts * h
           height = nbrPts * v
           summits = (fI nbrPts / nbrSummit)

       screen <- SDL.setVideoMode width height 32 [SDL.SWSurface]

       gen <- getStdGen
       let (seed,_) = random gen
           --(seed,_) = random.mkStdGen $ 12
           
           p (i,j) = perlin2D (i,j) oct per seed        

           pts = [(i,j,p ((fI i)/summits,(fI j)/summits)) | i <- [0..nbrPts-1], j <- [0..nbrPts-1]]
           
           vals = map (\(x,y,n) -> n) pts
           (mini,maxi) = (List.minimum vals, List.maximum vals)

           toScale v = let v' = nbrCol * (v-mini)/(maxi-mini)
                           v'' = 255*(fI.round $ v')/nbrCol
                       in round v''
           
           toColorScale v = let v' = nbrCol * (v-mini)/(maxi-mini)
                                v'' = 300*(fI.round $ v')/nbrCol
                                hue = mod (180 + (round v'')) 360 
           	                in hsvToRgb (fI hue,1,1)

           pix = if col 
           	     then map (\(x,y,n) -> (fI x,fI y,getPixel.toColorScale $ n)) pts
           	     else let go n = let sn = (fI.toScale $ n) in (sn,sn,sn)
           	          in map (\(x,y,n) -> (fI x,fI y,getPixel.go $ n)) pts
           
           zoomedPix = zoomPoly (fI h) (fI v) pix
       
       sequence $ map (\(pts,p) -> SDLP.filledPolygon screen pts p) zoomedPix
       SDL.flip screen
       

       --let mapped = map (\(x,y,n) -> (fI.toScale $ n)) pts

       putStrLn $ "Nbr of distinct values: " ++ show (Set.size $ Set.fromList vals)
       putStrLn $ "Val min/max: " ++ show (mini, maxi)
       putStrLn $ "Possible Max " ++ show (if per >= 1 then (fI oct*per^oct) else sum [per ^ i | i <- [0..oct]]) 
       putStrLn $ "Delta: " ++ show (maxi - mini)

       --putStrLn $ "Nbr of distinct mapped values: " ++ show (Set.size $ Set.fromList mapped)
       --putStrLn $ "Val min/max: " ++ show (List.minimum mapped, List.maximum mapped)
       
       let loop = do SDL.delay 10
       	             event      <- SDL.pollEvent
                     case event of
                          SDL.Quit -> return ()
                          SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _) -> return ()
                          SDL.NoEvent -> loop 
                          _       -> loop
       loop

      
       
