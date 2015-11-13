module Tiles where
import Control.DeepSeq
import Foreign.C.Types

data ChunkType = Islands | Continent | Mountains

data WaterType = Shallow
               | Deep1
               | Deep2
               | Swirl1
               | Swirl2
               | Swirl3
               | Swirl4 deriving (Show,Eq)

               
data LandType = Swamp
              | GrassLand
              | Bushes
              | Dirt
              | Thicket
              | SmallTrees
              | Forest
              | LowMountain 
              | HighMountain1
              | HighMountain2
              | Rocks1
              | Rocks2 
              | UpBorder
              | DownBorder
              | LeftBorder
              | RightBorder
              | UpLeftBorder
              | UpRightBorder
              | DownLeftBorder
              | DownRightBorder deriving (Show,Eq)

data BeingType  = Mage1
                | Mage2
                | Mage3
                | Mage4
                | Shark1
                | Shark2
                | Shark3
                | Shark4 deriving (Show,Eq)

data BuildingType = Tower
                  | SmallCastle1
                  | SmallCastle2
                  | Shrine deriving (Show,Eq)


data TransportType = Boat1
                   | Boat2
                   | Boat3
                   | Boat4
                   | Boat5
                   | Boat6
                   | Boat7
                   | Boat8 deriving (Show,Eq)

data FurnitureType = Fountain1
                   | Fountain2
                   | Fountain3
                   | Fountain4 deriving (Show,Eq)

data Tile = Water WaterType 
          | Land LandType
          | Being BeingType
          | Transport TransportType
          | Building BuildingType
          | Furniture FurnitureType deriving (Show,Eq)

type TileStack = [Tile]

instance NFData Tile where rnf x = seq x ()

slow :: Int -> [a] -> [a]
slow 0 xs = xs
slow n [] = []
slow n (x:xs) = go n x [] ++ (slow n xs) where go 0 _ xs = xs
                                               go n x xs = go (n-1) x (x:xs)

getTileCoord :: Tile -> (CInt,CInt)
getTileCoord t = 
  let (i,j) = case t of Water Shallow -> (0,3)
                        Water Deep1 -> (0,1)
                        Water Deep2 -> (0,2)
                        Water Swirl1 -> (15,12)
                        Water Swirl2 -> (15,13)
                        Water Swirl3 -> (15,14)
                        Water Swirl4 -> (15,15)

                        Land Swamp -> (0,4)
                        Land GrassLand -> (0,5)
                        Land Bushes -> (0,6)
                        Land Dirt -> (0,7)
                        Land Thicket -> (0,8)
                        Land HighMountain1 -> (0,12)
                        Land HighMountain2 -> (0,13)
                        Land LowMountain -> (0,11)
                        Land SmallTrees -> (0,9)
                        Land Forest -> (0,10)
                        Land Rocks1 -> (0,14)
                        Land Rocks2 -> (0,15)
                        Land UpBorder -> (1,16)
                        Land DownBorder -> (1,18)
                        Land LeftBorder -> (1,19)
                        Land RightBorder -> (1,17)
                        Land UpLeftBorder -> (1,20)
                        Land UpRightBorder -> (1,21)
                        Land DownLeftBorder -> (1,23)
                        Land DownRightBorder -> (1,22)

                        Being Mage1 ->(10,0)
                        Being Mage2 ->(10,1)
                        Being Mage3 ->(10,2)
                        Being Mage4 ->(10,3)
                        Being Shark1 -> (12,12)
                        Being Shark2 -> (12,13)
                        Being Shark3 -> (12,14)
                        Being Shark4 -> (12,15)

                        Building SmallCastle1 -> (0,20)
                        Building SmallCastle2 -> (0,21)
                        Building Tower -> (0,27)
                        Building Shrine -> (0,23)

                        Furniture Fountain1 -> (6,24)
                        Furniture Fountain2 -> (6,25)
                        Furniture Fountain3 -> (6,26)
                        Furniture Fountain4 -> (6,27)
        in (j*32,i*32) 


noise2Tile :: ChunkType -> Int -> [TileStack]
noise2Tile (Continent) n | n == 0 = cycle $ [[Water Deep2]]
                         | n == 1 = cycle $ [[Water Deep2]]
                         | n == 2 = cycle [[Land Swamp]]
                         | n == 3 = cycle [[Land GrassLand]]
                         | n == 4 = cycle [[Land SmallTrees]]
                         | n == 5 = cycle [[Land Forest]]
                         | n == 6 = cycle [[Land LowMountain]]
                         | n == 7 = cycle [[Land HighMountain1]]

noise2Tile (Islands) n   | elem n [0..6] = cycle $ [[Water Deep2]]
                         | n == 7 = cycle [[Land Swamp]]
                         | elem n [8..9] = cycle [[Land GrassLand]]
                         | n == 10 = cycle [[Land SmallTrees]]
                         | n == 11 = cycle [[Land Forest]]
                         | n == 12 = cycle [[Land LowMountain]]
                         | n == 13 = cycle [[Land HighMountain2]]

noise2Tile (Mountains) n | elem n [0..6] = cycle $ [[Land HighMountain2]]
                         | n == 7 = cycle [[Land HighMountain1]]
                         | elem n [8..9] = cycle [[Land Rocks1]]
                         | n == 10 = cycle [[Land SmallTrees]]
                         | n == 11 = cycle [[Land GrassLand]]
                         | n == 12 = cycle [[Land Dirt]]
                         | n == 13 = cycle [[Water Deep2]]

isNotWalkable :: Tile -> Bool
isNotWalkable (Land HighMountain2) = True
isNotWalkable (Water _) = True
isNotWalkable _ = False

maje = let m1 = slow 20 [[Being Mage1]]
           m2 = slow 10 [[Being Mage2]]
           m3 = slow 10 [[Being Mage3]]
           m4 = slow 10 [[Being Mage4]]
        in cycle $ m1 ++ m2 ++ m3 ++ m4


isWater :: Tile -> Bool
isWater (Water _) = True
isWater _ = False

isLand :: Tile -> Bool
isLand (Land _) = True
isLand _ = False

getGround :: [TileStack] -> Tile
getGround (x:xs) = head x

setGround :: [TileStack] -> Tile -> [TileStack]
setGround ((x':xs'):xs) g = cycle [(g:xs')] 

sameKind :: Tile -> Tile -> Bool
sameKind (Water _) (Water _) = True
sameKind (Land _) (Land _) = True
sameKind (Being _) (Being _) = True
sameKind (Transport _) (Transport _) = True
sameKind _ _= False

notSameKind :: Tile -> Tile -> Bool
notSameKind t1 t2 = not $ sameKind t1 t2

