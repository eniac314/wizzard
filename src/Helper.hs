module Helper where
import EngineTypes
import qualified Data.List as List
import Tiles hiding (slow)
import Vector (Mat,(§),vmap')
import qualified Data.Map.Strict as Map
import System.Random
import Data.Maybe (fromJust)
import Noise (noiseMat)





{- Chunk getters/setters -}
getCanvasPos :: World -> (Int,Int)
getCanvasPos = canPos . chunk

getTiles :: World -> Mat [[Tile]]
getTiles = chLand . chunk 

getChunkSize :: World -> Int
getChunkSize = chunkSize . chunk 

getCanvasSize :: World -> (Int,Int)
getCanvasSize = canvasSize . chunk

setTile ::  World -> Mat [TileStack] -> World
setTile w t = w {chunk = (chunk w) {chLand = t}}

setCanPos :: World -> (Int,Int) -> World
setCanPos w (x,y) = w {chunk = (chunk w){canPos = (x,y)}} 

{- Player getters/setters -}
getPlayerPos :: World -> (Int,Int)
getPlayerPos = plPos . player 

getPlayerTiles :: World -> [TileStack]
getPlayerTiles = plTiles . player

changeDir :: World -> Direction -> World
changeDir w d =  w { player = (player w) {direct = d}}

printPlayerData :: World -> IO ()
printPlayerData w = let p = player w
                        (x,y) = plPos p
                        d = direct p
                    in print (x,y,d)

-------------------------------------------------------------------------------------------------
{- Misc -}

osc :: Int -> Int -> Int -> [Int]
osc a b c = cycle $ [a,a+c..b] ++ [b,b-c..a+c]

slow :: Int -> [a] -> [a]
slow 0 xs = xs
slow n [] = []
slow n (x:xs) = go n x [] ++ (slow n xs) where go 0 _ xs = xs
                                               go n x xs = go (n-1) x (x:xs)

whnfList xs = List.foldl' (flip seq) () xs `seq` xs

fI = fromIntegral

deleteAllBy :: (a -> Bool) -> [a] -> [a]
deleteAllBy p = filter (not.p)

--returns nbrVal values choosen randomly from source and satisfying p
randIf :: [a] -> Int -> Seed -> (a -> Bool) -> [a]
randIf source nbrVal seed p = 
    let index = toMap source
        gen = mkStdGen seed
        vals = (randomRs (0,(length source) - 1) gen)
    in take nbrVal $ List.filter p [index Map.! v | v <- vals]


toMap :: [a] -> (Map.Map Int a)
toMap xs = Map.fromList (zip [0..] xs)

surroundings :: World -> (Int,Int) -> [Tile]
surroundings w (x,y) = let lnd = getTiles w
                           n = getChunkSize w
                       in [getGround (lnd § (n*i+j)) | i <- [(y-1)..(y+1)], j <- [(x-1)..(x+1)]]


makeLand :: ChunkType -> Int -> Seed -> Mat [TileStack]
makeLand (Continent) n s = vmap' (noise2Tile Continent) (noiseMat 18 0.2 n 25 7 s)
makeLand (Islands) n s = vmap' (noise2Tile Islands) (noiseMat 8 0.2 n 20 13 s)
makeLand (Mountains) n s = vmap' (noise2Tile Mountains) (noiseMat 8 0.2 n 20 13 s)


---------------------------------------------------------------------------------------------------
{- Path Finder -}

pathFinder :: World -> (Int,Int) -> (Int,Int) -> (Int) -> [(Int,Int)]
pathFinder w start@(i1,j1) goal@(i2,j2) maxSteps = 
    let 

        -- checks if there is the same cell in mainlist with a lower indice
        sel _ [] = False
        sel (a,b,n) ((c,d,m):xs) | (a,b) == (c,d) && m <= n = True 
                                 | otherwise = sel (a,b,n) xs 
        
        -- returns true if out of bound, not walkable or existing in mainlist with lower indice   
        p mainList = (\(i,j,n) ->   (maxBound*i+j) < 0 || maxBound*i+j >= (maxBound*maxBound - 1)
                                    || (isNotWalkable.getGround $ (land § (maxBound*i+j)))
                                    || sel (i,j,n) mainList)

        prune candidates mainList = deleteAllBy (p mainList) candidates
        
        -- tiles the chunk from start to goal until goal is found or n == 0
        computeMainList 0 ml xs  = []
        computeMainList _ _ [] = []
        computeMainList n ml (x:xs) = let cand  = neighbours x
                                          cand' = prune cand ml
                                      in if any (\(a,b,n) -> (a,b) == goal) cand
                                         then (i2,j2,n+1):ml
                             	         else computeMainList (n-1) (cand' ++ ml) (xs ++ cand')
        
        -- goes though mainlist from goal to start, removing all superfluous cells   
        -- returns the path in correct order (start -> goal)
        makePath [] r = r
        makePath xs (r:rs) = let nextPos = minPos r xs
                                 ns = dropWhile (\e -> e /= nextPos) xs
                             in case ns of [] -> (nextPos:r:rs)
                                           _  -> makePath (tail ns) (nextPos:r:rs)
        
        mainList = computeMainList maxSteps [(i1,j1,0)] [(i1,j1,0)]
        path | mainList == [] = []
             | otherwise = makePath (tail mainList) [(head mainList)] 
    
    in map (\(i,j,n) -> (i,j)) path where
        -- finds the neighbours of x in mainlist, and return the one with the lowest indice 
        minPos x xs = let nearby = filter (\e -> any (customEq e) (neighbours x)) (xs)
                      in case nearby of [] -> x
                                        _  -> List.foldl' minBy (head nearby) nearby

        minBy (a,b,c) (e,f,g) | c <= g = (a,b,c)
                              | otherwise = (e,f,g)


        customEq (a,b,c) (e,f,d) | a == e && b == f = True
                                 | otherwise = False

        land = getTiles w
        maxBound = getChunkSize w
        
        neighbours (i,j,n) = [(i-1,j,n+1),(i,j-1,n+1),(i+1,j,n+1),(i,j+1,n+1)]
        

