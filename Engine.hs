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

addThings :: Int-> Mat [[Tile]] -> [(Int,Int,Tile)] -> Mat [[Tile]]
addThings n m xs = runST $ do m' <- Vec.thaw m
                              let loop [] = return ()
                                  loop ((i,j,t):xs) = do let ind = i * n + j
                                                         tileStack <- GM.read m' ind
                                                         let t' = [(head tileStack) ++ [t]] 
                                                         GM.write m' ind (cycle (deepseq t' t'))
                                                         loop xs
                              loop xs
                              Vec.freeze m'
    
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

      addBorder t (y,x) =    
       let (ul:u:ur:l:g:r:dl:d:dr:[]) = [getGround (lnd § (n*i+j)) | i <- [(y-1)..(y+1)], j <- [(x-1)..(x+1)]]
       in if sameKind l r
          then if sameKind u d
               then (y,x,t)
               else if isWater u then (y,x,setGround t (Land UpBorder)) else (y,x,setGround t (Land DownBorder)) 
          else if isWater l 
               then if isWater d then (y,x,setGround t (Land DownLeftBorder))
                    else if isWater u then (y,x,setGround t (Land UpLeftBorder))
                      else (y,x,setGround t (Land LeftBorder))
               else if isWater d then (y,x,setGround t (Land DownRightBorder))
                    else if isWater u then (y,x,setGround t (Land UpRightBorder))
                      else (y,x,setGround t (Land RightBorder)) 
  
      changes =  [addBorder (lnd § (n*i+j)) (i,j) | i <- [1..(n-2)], j <- [1..(n-2)], not.isWater.getGround $ (lnd § (n*i+j))]

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
 
----------------------------------------------------------------------------------------------------
     