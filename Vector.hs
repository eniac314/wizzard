module Vector where
import qualified Data.Vector as Vec
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.List as List
import Control.Monad.ST 


type Mat a = Vec.Vector a

--printVec :: (Show a) => Mat a -> String
--printVec v | (Vec.length $ Vec.tail v) == 0 = drop 9 $ (show $ Vec.head v)
--           | otherwise = drop 9 $ (show $ Vec.head v) ++ '\n':printVec (Vec.tail v)



whnfElements :: Vec.Vector a -> Vec.Vector a
whnfElements v = Vec.foldl' (flip seq) () v `seq` v

vmap' :: (a -> b) -> Vec.Vector a -> Vec.Vector b
vmap' f = whnfElements . Vec.map f

vImap' :: (Int -> a -> b) -> Vec.Vector a -> Vec.Vector b
vImap' f = whnfElements.Vec.imap f

updates :: Int-> Mat a -> [(Int,Int,a)] -> Mat a
updates n m xs = List.foldl' (update n) m xs

update :: Int -> Mat a -> (Int,Int,a) -> Mat a
update n m (i,j,v) = runST $ do m' <- Vec.thaw m
                                GM.write m' (i*n+j) v
                                Vec.freeze m'

