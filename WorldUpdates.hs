module WorldUpdates where
import System.Random
import Helper
import Tiles hiding (slow)
import EngineTypes
import Vector
import qualified Data.List as List
import qualified Data.Vector as Vec
import qualified Data.Vector.Generic.Mutable as GM
import Control.Monad.ST
import Control.DeepSeq

{-- World update --}

-- Takes a list of (pos,Tile) (not cyclic!) and add a new tile on top of the tile stack 
addThings :: World -> [(Int,Int,Tile)] -> World
addThings w xs = 
  let n = getChunkSize w
      m = getTiles w
      lnd = runST $ do m' <- Vec.thaw m
                       let loop [] = return ()
                           loop ((i,j,t):xs) = do let ind = i * n + j
                                                  tileStack <- GM.read m' ind
                                                  let t' = [(head tileStack) ++ [t]] 
                                                  GM.write m' ind (cycle (deepseq t' t'))
                                                  loop xs
                       loop xs
                       Vec.freeze m'
  in w {chunk = (chunk w) {chLand = lnd}}


updateTiles :: World -> [(Int,[TileStack])] -> Bool -> World
updateTiles w xs erase | erase = updateTiles' w xs
                       | otherwise = updateTiles'' w xs

-- Takes a list of (pos,[tileStack]) (not cyclic!) and replace cells at pos with them    
updateTiles' :: World -> [(Int,[TileStack])] -> World
updateTiles' w xs =
  let n = getChunkSize w
      m = getTiles w
      lnd = runST $ do m' <- Vec.thaw m
                       let loop [] = return ()
                           loop ((ind,t):xs) = do GM.write m' ind (cycle (deepseq t t))
                                                  loop xs
                       loop xs
                       Vec.freeze m'
  in w {chunk = (chunk w) {chLand = lnd}}

-- Takes a list of (pos,[tileStack]) (not cyclic!) and add tiles on top of the existing ones 
updateTiles'' :: World -> [(Int,[TileStack])] -> World
updateTiles'' w xs =
  let n = getChunkSize w
      m = getTiles w
      lnd = runST $ do m' <- Vec.thaw m
                       let loop [] = return ()
                           loop ((ind,t):xs) = do oldStackList <- GM.read m' ind
                                                  let t' = map (\ts -> (head oldStackList) ++ ts) t 
                                                  GM.write m' ind (cycle (deepseq t' t'))
                                                  loop xs
                       loop xs
                       Vec.freeze m'
  in w {chunk = (chunk w) {chLand = lnd}}

-- Goes through the whole chunk and update each cell's animation list
updateTail ::  World -> World
updateTail w = 
  let nbr = getChunkSize w
      m  = getTiles w
      m' = runST $ do m' <- Vec.thaw (m)
                      let loop 0 = return ()
                          loop n = do (x:xs) <- GM.read m' n
                                      deepseq x (GM.write m' n xs)
                                      loop (n-1)
                      loop ((nbr*nbr - 1))
                      Vec.freeze m'
  in setTile w m'

updatePlayerTiles :: World -> World
updatePlayerTiles w = let t = getPlayerTiles w
                      in w {player = (player w) {plTiles = tail t}}

-- remplace water edges by borders and shallow water 
addBorders :: World -> World
addBorders w = 
  let lnd = getTiles w
      n = getChunkSize w
      
      addBorder t (y,x) | sameKind l r && sameKind u d = []
                        | sameKind l r && isWater u = (y,x,up):[(y - 1, x, shallow)]
                        | sameKind l r = (y,x,down):[(y + 1, x, shallow)]
                        | isWater l && isWater d = (y,x,downLeft):[(y, x - 1, shallow)]
                        | isWater l && isWater u = (y,x,upLeft):[(y, x -1 , shallow)]
                        | isWater l = (y,x,left):[(y,x - 1,shallow)]
                        | isWater d = (y,x,downRight):[(y, x + 1, shallow)]
                        | isWater u = (y,x,upRight):[(y, x + 1, shallow)]
                        | otherwise = (y,x,right):[(y, x + 1, shallow)]

        where (ul:u:ur:l:g:r:dl:d:dr:[]) = surroundings w (x,y)
              (up, down) = (setGround t (Land UpBorder),setGround t (Land DownBorder))
              (left, right) = (setGround t (Land LeftBorder),setGround t (Land RightBorder))
              (upLeft,downLeft) = (setGround t (Land UpLeftBorder),setGround t (Land DownLeftBorder))
              (upRight,downRight) = (setGround t (Land UpRightBorder),setGround t (Land DownRightBorder))
              shallow = setGround t (Water Shallow)

  
      changes = concat [addBorder (lnd § (n*i+j)) (i,j) | i <- [1..(n-2)], j <- [1..(n-2)], not.isWater.getGround $ (lnd § (n*i+j))]

  in  updateTiles w  (map (\(i,j,t) -> (i*n+j,[head t])) changes) True           

-- add the object represented by [TileStack] nbrValue times at places satisfaying p 
addRandom :: World -> [TileStack]-> (Int -> Bool) -> Int -> Seed-> Bool -> World
addRandom w ts p nbrValue seed erase = 
  let maxBound = getChunkSize w
      coords = [0..(maxBound * maxBound) - 1]
      vals = randIf coords nbrValue seed p
      changes = map (\ind -> (ind,ts)) vals
  in updateTiles w changes erase 

addTower :: Int -> Seed -> World -> World
addTower n s w = 
  let lnd = getTiles w
      f = [[Building Tower]]
  in addRandom w f (\ind -> (Land LowMountain) == (getGround (lnd § ind))) n s False

addShrine :: Int -> Seed -> World -> World
addShrine n s w = 
  let lnd = getTiles w
      f = [[Building Shrine]]
  in addRandom w f (\ind -> (Land LowMountain) == (getGround (lnd § ind))) n s False

addSmallCastle1 :: Int -> Seed -> World -> World
addSmallCastle1 n s w = 
  let lnd = getTiles w
      f = [[Building SmallCastle1]]
  in addRandom w f (\ind -> (Land Forest) == (getGround (lnd § ind))) n s False

addSmallCastle2 :: Int -> Seed -> World -> World
addSmallCastle2 n s w = 
  let lnd = getTiles w
      f = [[Building SmallCastle2]]
  in addRandom w f (\ind -> (Land SmallTrees) == (getGround (lnd § ind))) n s False

addFountain :: Int -> Seed -> World -> World
addFountain n s w = 
  let lnd = getTiles w
      m1 = slow 5 [[Furniture Fountain1]]
      m2 = slow 5 [[Furniture Fountain2]]
      m3 = slow 5 [[Furniture Fountain3]]
      m4 = slow 5 [[Furniture Fountain4]]
      f = m1 ++ m2 ++ m3 ++ m4
  in addRandom w f (\ind -> (Land GrassLand) == (getGround (lnd § ind))) n s False

addShark :: Int -> Seed -> World -> World
addShark n s w = 
  let lnd = getTiles w
      m1 = slow 15 [[Being Shark1]]
      m2 = slow 15 [[Being Shark2]]
      m3 = slow 15 [[Being Shark3]]
      m4 = slow 25 [[Being Shark4]]
      f = m1 ++ m2 ++ m3 ++ m4
  in addRandom w f (\ind -> (Water Deep2) == (getGround (lnd § ind))) n s False

addSwirl :: Int -> Seed -> World -> World
addSwirl n s w = 
  let lnd = getTiles w
      m1 = slow 10 [[Water Swirl1]]
      m2 = slow 10 [[Water Swirl2]]
      m3 = slow 10 [[Water Swirl3]]
      m4 = slow 10 [[Water Swirl4]]
      f = m1 ++ m2 ++ m3 ++ m4
  in addRandom w f (\ind -> (Water Deep2) == (getGround (lnd § ind))) n s False

addVarious :: Seed -> World -> World
addVarious s w = 
  let gen = mkStdGen s
      funcs1 = [(addShark 5), (addFountain 10)
               ,(addSwirl 4), (addTower 5)
               ,(addShrine 3), (addSmallCastle1 6)
               ,(addSmallCastle2 10)]
      
      funcs2 [] _ = []
      funcs2 (f:fs) g = let (s,g') = random g
                        in (f s):funcs2 fs g'

      func = List.foldl' (\f1 f2 -> f1.f2) id (funcs2 funcs1 gen)
  in func w

-------------------------------------------------------------------------------------
testPathFinder :: (Int,Int) -> (Int,Int) -> (Int) -> World -> World
testPathFinder s g m w = let p = pathFinder w s g m
                             changes = map (\(x,y) -> (x,y,Building Tower)) p
                             n = getChunkSize w
                             lnd = getTiles w
                         in addThings w changes