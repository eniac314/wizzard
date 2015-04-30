module Engine where
import Control.Monad
import Control.Monad.ST
import qualified Graphics.UI.SDL as SDL
import qualified Data.List as List
import qualified Data.Vector as Vec
import qualified Data.Vector.Generic.Mutable as GM
import Control.DeepSeq
import Tiles hiding (slow)
import Helper
import EngineTypes
import Vector
import Graphics




{-- World update --}

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
    
updateTiles :: Int -> Mat [[Tile]] -> [(Int,Int,[[Tile]])] -> Mat [[Tile]]
updateTiles n m xs = runST $ do m' <- Vec.thaw m
                                let loop [] = return ()
                                    loop ((i,j,t):xs) = do let ind = i * n + j
                                                           GM.write m' ind t
                                                           loop xs
                                loop xs
                                Vec.freeze m'

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

  in w {chunk = (chunk w) {chLand = updateTiles n lnd changes}}             

{-- Movments --}

moveCamera :: World -> World
moveCamera w  = let maxBound = (chunkSize.chunk $ w) - (uncurry max) (canvasSize.chunk $ w)
                    isInBounds (i,j) = (i >= 0 && i < maxBound) && (j >= 0 && j < maxBound)
                    (pX,pY) = getPlayerPos w
                    (canX,canY) = getCanvasPos w
                    (hei,wid) = getCanvasSize w
                    ch = chunk w
                    p = player w in
                 
                 case direct p of
                           Stop -> w
                           Lefty  -> if isInBounds (canX - 1, canY) && (pX - canX <= 3)
                                    then w {chunk = ch {canPos = (canX-1,canY)}}
                                    else w
                           Righty -> if isInBounds (canX + 1, canY) && (canX + wid - pX <= 3)
                                    then w {chunk = ch {canPos = (canX+1,canY)}}
                                    else w
                           Up    -> if isInBounds (canX, canY - 1) && (pY - canY <= 3)
                                    then w {chunk = ch {canPos = (canX,canY - 1)}}
                                    else w
                           Down  -> if isInBounds (canX, canY + 1) && (canY + hei - pY <= 3)
                                    then w {chunk = ch {canPos = (canX,canY + 1)}}
                                    else w

movePlayer :: World -> World
movePlayer w = let maxBound = getChunkSize w
                   n = getChunkSize w
                   isInBounds (i,j) = (i >= 0 && i < maxBound) && (j >= 0 && j < maxBound)
                   isWalkable (y,x) = not.isNotWalkable.head.head $ (l § (n*y+x))
                   l = getTiles w
                   (pX,pY) = getPlayerPos w
                   p = player w in
               
               case direct p of
                           Stop -> w
                           Lefty  -> if isInBounds (pX - 1, pY) && (isWalkable (pY,pX-1))                                   
                                     then w {player = p {plPos = (pX - 1, pY)}} 
                                     else w
                           Righty -> if isInBounds (pX + 1, pY) && (isWalkable (pY,pX+1))
                                     then w {player = p {plPos = (pX+1,pY)}}
                                     else w
                           Up    -> if isInBounds (pX, pY - 1) && (isWalkable (pY - 1,pX))
                                    then w {player = p {plPos = (pX,pY - 1)}}
                                    else w
                           Down  -> if isInBounds (pX, pY + 1) && (isWalkable (pY + 1,pX))
                                    then w {player = p {plPos = (pX,pY + 1)}}
                                    else w
 
 
testPathFinder :: (Int,Int) -> (Int,Int) -> (Int) -> World -> World
testPathFinder s g m w = let p = pathFinder w s g m
                             changes = map (\(x,y) -> (x,y,Building Tower)) p
                             n = getChunkSize w
                             lnd = getTiles w
                         in addThings w changes
----------------------------------------------------------------------------------------------------
     