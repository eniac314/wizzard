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
import Graphics hiding (nbrPts)


nbrPts = 200

{-- World update --}

addThings :: Mat [[Tile]] -> [(Int,Int,Tile)] -> Mat [[Tile]]
addThings m xs = runST $ do m' <- Vec.thaw m
                            let loop [] = return ()
                                loop ((i,j,t):xs) = do let ind = i * nbrPts + j
                                                       tileStack <- GM.read m' ind
                                                       let t' = [(head tileStack) ++ [t]] 
                                                       GM.write m' ind (cycle (deepseq t' t'))
                                                       loop xs
                            loop xs
                            Vec.freeze m'
    
updateTail ::  World -> World
updateTail w = 
  let m  = getTiles w
      m' = runST $ do m' <- Vec.thaw (m)
                      let loop 0 = return ()
                          loop n = do (x:xs) <- GM.read m' n
                                      deepseq x (GM.write m' n xs)
                                      loop (n-1)
                      loop ((nbrPts*nbrPts - 1))
                      Vec.freeze m'
  in setTile w m'

updatePlayerTiles :: World -> World
updatePlayerTiles w = let t = getPlayerTiles w
                      in w {player = (player w) {plTiles = tail t}}

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
movePlayer w = let maxBound = nbrPts
                   isInBounds (i,j) = (i >= 0 && i < maxBound) && (j >= 0 && j < maxBound)
                   (pX,pY) = getPlayerPos w
                   p = player w in
               
               case direct p of
                           Stop -> w
                           Lefty  -> if isInBounds (pX - 1, pY)                                     
                                     then w {player = p {plPos = (pX - 1, pY)}} 
                                     else w
                           Righty -> if isInBounds (pX + 1, pY)
                                     then w {player = p {plPos = (pX+1,pY)}}
                                     else w
                           Up    -> if isInBounds (pX, pY - 1)
                                    then w {player = p {plPos = (pX,pY - 1)}}
                                    else w
                           Down  -> if isInBounds (pX, pY + 1)
                                    then w {player = p {plPos = (pX,pY + 1)}}
                                    else w
 
----------------------------------------------------------------------------------------------------
     