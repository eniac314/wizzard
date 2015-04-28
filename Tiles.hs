module Tiles where
import Control.DeepSeq
import qualified Graphics.UI.SDL as SDL

data WaterType = Shallow
               | Deep1
               | Deep2
               | UpLeftDiag
               | UpRightDiag
               | DownLeftDiag
               | DownRightDiag deriving Show

data LandType = Swamp
              | GrassLand
              | Bushes
              | Thicket
              | SmallTrees
              | Forest
              | LowMountain 
              | HighMountain1
              | HighMountain2
              | Rocks1
              | Rocks2 
              | Field deriving Show

data BeingType  = Mage1
                | Mage2
                | Mage3
                | Mage4 deriving Show

data BuildingType = Tower
                  | SmallCastle1
                  | SmallCastle2 deriving Show


data TransportType = Boat1
                   | Boat2
                   | Boat3
                   | Boat4
                   | Boat5
                   | Boat6
                   | Boat7
                   | Boat8 deriving Show

data Tile = Water WaterType 
          | Land LandType
          | Being BeingType
          | Transport TransportType
          | Building BuildingType deriving Show

instance NFData Tile where rnf x = seq x ()

slow :: Int -> [a] -> [a]
slow 0 xs = xs
slow n [] = []
slow n (x:xs) = go n x [] ++ (slow n xs) where go 0 _ xs = xs
                                               go n x xs = go (n-1) x (x:xs)

getTileCoord :: Tile -> Maybe SDL.Rect
getTileCoord t = 
  let (i,j) = case t of Water Shallow -> (0,3)
                        Water Deep1 -> (0,1)
                        Water Deep2 -> (0,2)
                        Land Swamp -> (0,4)
                        Land GrassLand -> (0,5)
                        Land HighMountain1 -> (0,12)
                        Land HighMountain2 -> (0,13)
                        Land LowMountain -> (0,10)
                        Land SmallTrees -> (0,8)
                        Land Forest -> (0,9)
                        Being Mage1 ->(10,0)
                        Being Mage2 ->(10,1)
                        Being Mage3 ->(10,2)
                        Being Mage4 ->(10,3)
                        Building SmallCastle1 -> (0,20)
                        Building SmallCastle2 -> (0,21)
                        Building Tower -> (0,27)
                        _              -> (0,7)	                      
	in Just SDL.Rect { SDL.rectX = j * 32, SDL.rectY = i * 32 , SDL.rectW = 32, SDL.rectH = 32}

noise2Tile :: Int -> [[Tile]]
noise2Tile n | n == 0 = cycle $ slow 20 [[Water Deep2],[Water Deep1]]
             | n == 1 = cycle $ slow 20 [[Water Deep2],[Water Deep1]]
             | n == 2 = cycle [[Land Swamp]]
             | n == 3 = cycle [[Land GrassLand]]
             | n == 4 = cycle [[Land SmallTrees]]
             | n == 5 = cycle [[Land Forest]]
             | n == 6 = cycle [[Land LowMountain]]
             | n == 7 = cycle [[Land HighMountain1]]

maje = let m1 = slow 20 [[Being Mage1]]
           m2 = slow 10 [[Being Mage2]]
           m3 = slow 10 [[Being Mage3]]
           m4 = slow 10 [[Being Mage4]]
        in cycle $ m1 ++ m2 ++ m3 ++ m4