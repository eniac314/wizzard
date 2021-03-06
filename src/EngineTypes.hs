module EngineTypes where
import Tiles
import Foreign.C.Types
import SDL
import Vector (Mat)
import Control.Concurrent

data Avatar = Avatar { plPos :: (Int,Int)
                     , plTiles :: [TileStack]
                     , direct :: Direction
                     }

data Sys = Sys { width :: Int
               , height :: Int
               , randSeed :: Int
               , flagMVar :: MVar Bool
               , boxMVar :: MVar ([Mat [TileStack]])
               }



data Chunk = Chunk { chType :: ChunkType
                   , canPos :: (Int,Int)
                   , chLand :: Mat [TileStack]
                   , canvasSize :: (Int,Int)
                   , chunkSize :: Int
                   , chunkNbr :: Int
                   }

data World = World { sys :: Sys
                   , screen :: Renderer
                   , tileset :: Texture
                   , chunk :: Chunk
                   , chunks :: [Chunk]
                   , player :: Avatar
                   }

data Direction = Lefty | Righty| Up | Down | Stop deriving Show

type Seed = Int

