module Engine where
import Control.Monad
import Control.Monad.ST
import qualified Data.List as List
import Tiles hiding (slow)
import Helper
import EngineTypes
import Vector
import Graphics






-------------------------------------------------------------------------------------------------

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
                   Stop   -> w
                   Lefty  -> if isInBounds (canX - 1, canY) && (pX - canX <= 3)
                             then w {chunk = ch {canPos = (canX-1,canY)}}
                             else w
                   Righty -> if isInBounds (canX + 1, canY) && (canX + wid - pX <= 3)
                             then w {chunk = ch {canPos = (canX+1,canY)}}
                             else w
                   Up     -> if isInBounds (canX, canY - 1) && (pY - canY <= 3)
                             then w {chunk = ch {canPos = (canX,canY - 1)}}
                             else w
                   Down   -> if isInBounds (canX, canY + 1) && (canY + hei - pY <= 3)
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
                  Up     -> if isInBounds (pX, pY - 1) && (isWalkable (pY - 1,pX))
                            then w {player = p {plPos = (pX,pY - 1)}}
                            else w
                  Down   -> if isInBounds (pX, pY + 1) && (isWalkable (pY + 1,pX))
                            then w {player = p {plPos = (pX,pY + 1)}}
                            else w
 
 

----------------------------------------------------------------------------------------------------
{- Misc -}

