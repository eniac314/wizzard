module Helper where
import EngineTypes
import qualified Data.List as List
import Tiles (Tile)
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

getChunkPos :: World -> (Int,Int)
getChunkPos w = chPos.chunk $ w

getTiles :: World -> Mat [[Tile]]
getTiles w = chLand.chunk $ w

getPlayerPos :: World -> (Int,Int)
getPlayerPos w = plPos.player $ w

changeDir :: World -> Direction -> World
changeDir w d =  w { player = (player w) {direct = d}}

printPlayerData :: World -> IO ()
printPlayerData w = let p = player w
                        (x,y) = plPos p
                        d = direct p
                    in print (x,y,d) 