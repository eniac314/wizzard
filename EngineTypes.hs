module EngineTypes where
import Tiles
import qualified Graphics.UI.SDL.Framerate as SDLF
import qualified Graphics.UI.SDL as SDL
import Vector (Mat)

data Player = Player { plPos :: (Int,Int)
                     , plTiles :: [TileStack]
                     , direct :: Direction
                     }

data Sys = Sys { width :: Int
               , height :: Int
               , fps :: SDLF.FPSManager
               }



data Chunk = Chunk { chType :: ChunkType
                   , canPos :: (Int,Int)
                   , chLand :: Mat [TileStack]
                   , canvasSize :: (Int,Int)
                   , chunkSize :: Int
                   , chunkNbr :: Int
                   }

data World = World { sys :: Sys
                   , screen :: SDL.Surface
                   , tileset :: SDL.Surface
                   , chunk :: Chunk
                   , chunks :: [Chunk]
                   , player :: Player
                   }

data Direction = Lefty | Righty| Up | Down | Stop deriving Show

type Seed = Int

