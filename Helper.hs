module Helper where
import EngineTypes
import qualified Data.List as List
import Tiles hiding (slow)
import Vector (Mat,(§))



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


{- Misc -}
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

surroundings :: World -> (Int,Int) -> [Tile]
surroundings w (x,y) = let lnd = getTiles w
                           n = getChunkSize w
                       in [getGround (lnd § (n*i+j)) | i <- [(y-1)..(y+1)], j <- [(x-1)..(x+1)]]

{- Path Finder -}

pathFinder :: World -> (Int,Int) -> (Int,Int) -> (Int) -> [(Int,Int)]
pathFinder w start@(i1,j1) goal@(i2,j2) maxSteps = 
    let land = getTiles w
        maxBound = getChunkSize w
        
        minBy (a,b,c) (e,f,g) | c <= g = (a,b,c)
                              | otherwise = (e,f,g)

        neighbours (i,j,n) = [(i-1,j,n+1),(i,j-1,n+1),(i+1,j,n+1),(i,j+1,n+1)]

        sel _ [] = False
        sel (a,b,n) ((c,d,m):xs) | (a,b) == (c,d) && m <= n = True
                                 | otherwise = sel (a,b,n) xs 
          
        p mainList = (\(i,j,n) ->   (maxBound*i+j) < 0 || maxBound*i+j >= (maxBound*maxBound - 1)
                                    || (isNotWalkable.getGround $ (land § (maxBound*i+j)))
                                    || sel (i,j,n) mainList)

        prune candidates mainList = deleteAllBy (p mainList) candidates

        computeMainList 0 ml xs  = []
        computeMainList n ml (x:xs) = let cand  = neighbours x
                                          cand' = prune cand ml
                                      in if any (\(a,b,n) -> (a,b) == start) cand
                                         then (i1,j1,n+1):ml
                             	         else computeMainList (n-1) (cand' ++ ml) (xs ++ cand')


        minPos x@(_,_,n) xs = let nearby = filter (\e -> any (customEq e) (neighbours x)) (xs)
                                in case nearby of [] -> x
                                                  _  -> List.foldl' minBy (head nearby) nearby

        customEq (a,b,c) (e,f,d) | a == e && b == f = True
                                 | otherwise = False


        makePath [] r = r
        makePath xs (r@(_,_,n):rs) = let nextPos = minPos r xs
                                         ns = dropWhile (\e -> e /= nextPos) xs
                                     in case ns of [] -> (nextPos:r:rs)
                                                   _  -> makePath (tail ns) (nextPos:r:rs)
        
        mainList = computeMainList maxSteps [(i2,j2,0)] [(i2,j2,0)]
        path | mainList == [] = []
             | otherwise = makePath (tail mainList) [(head mainList)] 
    
    in map (\(i,j,n) -> (i,j)) path

