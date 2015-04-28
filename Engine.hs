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


nbrPts = 200

(§) :: Mat a -> (Int, Int) -> a
v § (r, c) = v Vec.! (r*nbrPts + c)


{-- Chunk update --}

putMageIn :: Mat [[Tile]] -> Mat [[Tile]]
putMageIn m = let tileStack = head $ m § (100,100) --[Tile]
                  m1 = slow 20 [tileStack ++ [Being Mage1]]
                  m2 = slow 10 [tileStack ++ [Being Mage2]]
                  m3 = slow 10 [tileStack ++ [Being Mage3]]
                  m4 = slow 10 [tileStack ++ [Being Mage4]]
              in update nbrPts m (100,100,cycle $ m1 ++ m2 ++ m3 ++ m4) 


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

addPlayer :: World -> World
addPlayer w = let p = player w
                  ((j,i),t) = (plPos p, plTiles p)
                  l = chLand.chunk $ w
                  ind = i * nbrPts + j
 
                  newl = runST $ do l' <- Vec.thaw l
                                    tileStack <- GM.read l' ind
                                    let t' = [(head tileStack) ++ [t]] 
                                    GM.write l' ind (cycle (deepseq t' t'))
                                    Vec.freeze l'
              in w {chunk = (chunk w){chLand = newl}}
                            
updateTail ::  Mat [[Tile]] -> Mat [[Tile]]
updateTail m = runST $ do m' <- Vec.thaw (m)
                          let loop 0 = return ()
                              loop n = do (x:xs) <- GM.read m' n
                                          deepseq x (GM.write m' n xs)
                                          loop (n-1)
                          loop ((nbrPts*nbrPts - 1))
                          Vec.freeze m'


{-- Screen drawing --}

applyTile :: [Tile] -> (Int,Int) -> SDL.Surface -> SDL.Surface -> IO [Bool]
applyTile ts (x,y) src dest =
    let offset = Just SDL.Rect { SDL.rectX = x, SDL.rectY = y, SDL.rectW = 32, SDL.rectH = 32}
        tileRects = map getTile ts
    in sequence $ map (\tr -> SDL.blitSurface src tr dest offset) tileRects

applyTileMat :: Chunk -> SDL.Surface -> SDL.Surface -> IO Chunk
applyTileMat ch src dest = 
  let m = chLand $! ch
      (x,y) = chPos ch
      (canW,canH) = canvasSize ch in

  do sequence $ [ applyTile (head (m § (i,j))) (32*(j-x), 32*(i-y)) src dest | i <- [y..(y+canH)], j <- [x..(x+canW)]]
     let m' = updateTail (m) 
     return ch { chLand = m' }

tileList :: Chunk -> SDL.Surface -> SDL.Surface -> IO Chunk
tileList ch src dest = applyTileMat ch src dest


{-- Movments --}

moveCamera :: World -> World
moveCamera w  = let maxBound = (chunkSize.chunk $ w) - (uncurry max) (canvasSize.chunk $ w)
                    isInBounds (i,j) = (i >= 0 && i < maxBound) && (j >= 0 && j < maxBound)
                    (chX,chY) = (chPos.chunk $ w)
                    ch = chunk w
                    p = player w in
                 
                 case direct p of
                           Stop -> w
                           Lefty  -> if isInBounds (chX - 1, chY)
                                    then w {chunk = ch {chPos = (chX-1,chY)}}
                                    else w
                           Righty -> if isInBounds (chX + 1, chY)
                                    then w {chunk = ch {chPos = (chX+1,chY)}}
                                    else w
                           Up    -> if isInBounds (chX, chY - 1)
                                    then w {chunk = ch {chPos = (chX,chY - 1)}}
                                    else w
                           Down  -> if isInBounds (chX, chY + 1)
                                    then w {chunk = ch {chPos = (chX,chY + 1)}}
                                    else w

movePlayer :: World -> World
movePlayer w = let maxBound = nbrPts
                   isInBounds (i,j) = (i >= 0 && i < maxBound) && (j >= 0 && j < maxBound)
                   (pX,pY) = (plPos.player $ w)
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
     