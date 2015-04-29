module Helper where
import EngineTypes
import qualified Data.List as List
import Tiles hiding (slow)
import Vector (Mat)



osc :: Int -> Int -> Int -> [Int]
osc a b c = cycle $ [a,a+c..b] ++ [b,b-c..a+c]

slow :: Int -> [a] -> [a]
slow 0 xs = xs
slow n [] = []
slow n (x:xs) = go n x [] ++ (slow n xs) where go 0 _ xs = xs
                                               go n x xs = go (n-1) x (x:xs)

whnfList xs = List.foldl' (flip seq) () xs `seq` xs

fI = fromIntegral

{- Chunk getters/setters -}
getCanvasPos :: World -> (Int,Int)
getCanvasPos w = canPos.chunk $ w

getTiles :: World -> Mat [[Tile]]
getTiles w = chLand.chunk $ w

getChunkSize :: World -> Int
getChunkSize w = chunkSize.chunk $ w

getCanvasSize :: World -> (Int,Int)
getCanvasSize w = canvasSize.chunk $ w

setTile ::  World -> Mat [[Tile]] -> World
setTile w t = w {chunk = (chunk w) {chLand = t}}

setCanPos :: World -> (Int,Int) -> World
setCanPos w (x,y) = w {chunk = (chunk w){canPos = (x,y)}} 

{- Player getters/setters -}
getPlayerPos :: World -> (Int,Int)
getPlayerPos w = plPos.player $ w

getPlayerTiles :: World -> [[Tile]]
getPlayerTiles w = plTiles.player $ w

changeDir :: World -> Direction -> World
changeDir w d =  w { player = (player w) {direct = d}}

printPlayerData :: World -> IO ()
printPlayerData w = let p = player w
                        (x,y) = plPos p
                        d = direct p
                    in print (x,y,d)

isWater :: Tile -> Bool
isWater (Water _) = True
isWater _ = False

isLand :: Tile -> Bool
isLand (Land _) = True
isLand _ = False

getGround :: [[Tile]] -> Tile
getGround (x:xs) = head x

setGround :: [[Tile]] -> Tile -> [[Tile]]
setGround ((x':xs'):xs) g = cycle [(g:xs')] 

sameKind :: Tile -> Tile -> Bool
sameKind (Water _) (Water _) = True
sameKind (Land _) (Land _) = True
sameKind (Being _) (Being _) = True
sameKind (Transport _) (Transport _) = True
sameKind _ _= False

notSameKind :: Tile -> Tile -> Bool
notSameKind t1 t2 = not $ sameKind t1 t2