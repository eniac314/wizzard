{-# LINE 1 "Graphics/UI/SDL/Types.hsc" #-}

{-# LINE 2 "Graphics/UI/SDL/Types.hsc" #-}

{-# LINE 5 "Graphics/UI/SDL/Types.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Types
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Types
    ( SurfaceStruct
    , Surface
    , VideoInfoStruct
    , VideoInfo
    , RWopsStruct
    , RWops
    , PixelFormatStruct
    , PixelFormat
    , JoystickStruct
    , Joystick
    , Hat(..)
    , TimerIDStruct
    , SurfaceFlag (..)
    , surfaceGetPixelFormat
    , surfaceGetWidth
    , surfaceGetHeight
    , surfaceGetFlags
    , surfaceGetPitch
    , surfaceGetPixels
    , pixelFormatGetAlpha
    , pixelFormatGetColorKey
    , pixelFormatGetBitsPerPixel
    , pixelFormatGetBytesPerPixel
    , videoInfoWidth
    , videoInfoHeight
    ) where

import Foreign.C (CInt)
import Foreign (Word8, Word16, Word32, Ptr, Storable(peekByteOff),
                newForeignPtr_, ForeignPtr, withForeignPtr)
import System.IO.Unsafe (unsafePerformIO)

import Graphics.UI.SDL.Utilities (Enum(..), fromBitmask)
import Graphics.UI.SDL.Color (Pixel(..))

import Prelude hiding (Enum(..))

data SurfaceStruct
type Surface = ForeignPtr SurfaceStruct

data VideoInfoStruct
type VideoInfo = ForeignPtr VideoInfoStruct

data RWopsStruct
type RWops = ForeignPtr RWopsStruct

data PixelFormatStruct
type PixelFormat = ForeignPtr PixelFormatStruct

data TimerIDStruct

data PixelsData
type Pixels = Ptr PixelsData

data JoystickStruct
type Joystick = ForeignPtr JoystickStruct

data Hat
    = HatCentered
    | HatUp
    | HatRight
    | HatDown
    | HatLeft
    | HatRightUp
    | HatRightDown
    | HatLeftUp
    | HatLeftDown
      deriving (Show,Eq,Ord)

instance Bounded Hat where
    minBound = HatCentered
    maxBound = HatLeftDown

instance Enum Hat Word8 where
    fromEnum HatCentered = 0
{-# LINE 92 "Graphics/UI/SDL/Types.hsc" #-}
    fromEnum HatUp = 1
{-# LINE 93 "Graphics/UI/SDL/Types.hsc" #-}
    fromEnum HatRight = 2
{-# LINE 94 "Graphics/UI/SDL/Types.hsc" #-}
    fromEnum HatDown = 4
{-# LINE 95 "Graphics/UI/SDL/Types.hsc" #-}
    fromEnum HatLeft = 8
{-# LINE 96 "Graphics/UI/SDL/Types.hsc" #-}
    fromEnum HatRightUp = 3
{-# LINE 97 "Graphics/UI/SDL/Types.hsc" #-}
    fromEnum HatRightDown = 6
{-# LINE 98 "Graphics/UI/SDL/Types.hsc" #-}
    fromEnum HatLeftUp = 9
{-# LINE 99 "Graphics/UI/SDL/Types.hsc" #-}
    fromEnum HatLeftDown = 12
{-# LINE 100 "Graphics/UI/SDL/Types.hsc" #-}
    toEnum 0 = HatCentered
{-# LINE 101 "Graphics/UI/SDL/Types.hsc" #-}
    toEnum 1 = HatUp
{-# LINE 102 "Graphics/UI/SDL/Types.hsc" #-}
    toEnum 2 = HatRight
{-# LINE 103 "Graphics/UI/SDL/Types.hsc" #-}
    toEnum 4 = HatDown
{-# LINE 104 "Graphics/UI/SDL/Types.hsc" #-}
    toEnum 8 = HatLeft
{-# LINE 105 "Graphics/UI/SDL/Types.hsc" #-}
    toEnum 3 = HatRightUp
{-# LINE 106 "Graphics/UI/SDL/Types.hsc" #-}
    toEnum 6 = HatRightDown
{-# LINE 107 "Graphics/UI/SDL/Types.hsc" #-}
    toEnum 9 = HatLeftUp
{-# LINE 108 "Graphics/UI/SDL/Types.hsc" #-}
    toEnum 12 = HatLeftDown
{-# LINE 109 "Graphics/UI/SDL/Types.hsc" #-}
    toEnum _ = error "Graphics.UI.SDL.Types.toEnum: bad argument"
    succ HatCentered = HatUp
    succ HatUp = HatRight
    succ HatRight = HatDown
    succ HatDown = HatLeft
    succ HatLeft = HatRightUp
    succ HatRightUp = HatRightDown
    succ HatRightDown = HatLeftUp
    succ HatLeftUp = HatLeftDown
    succ _ = error "Graphics.UI.SDL.Types.succ: bad argument"
    pred HatUp = HatCentered
    pred HatRight = HatUp
    pred HatDown = HatRight
    pred HatLeft = HatDown
    pred HatRightUp = HatLeft
    pred HatRightDown = HatRightUp
    pred HatLeftUp = HatRightDown
    pred HatLeftDown = HatLeftUp
    pred _ = error "Graphics.UI.SDL.Types.pred: bad argument"
    enumFromTo x y | x > y = []
                   | x == y = [y]
                   | True = x : enumFromTo (succ x) y
    

data SurfaceFlag
    = SWSurface
    | HWSurface
    | OpenGL
    | ASyncBlit
    | OpenGLBlit
    | Resizable
    | NoFrame
    | HWAccel
    | SrcColorKey
    | RLEAccel
    | SrcAlpha
    | PreAlloc
    | AnyFormat
    | HWPalette
    | DoubleBuf
    | Fullscreen
    deriving (Eq, Ord, Show, Read)
instance Bounded SurfaceFlag where
      minBound = SWSurface
      maxBound = Fullscreen
instance Enum SurfaceFlag Word32 where
      fromEnum SWSurface = 0
      fromEnum HWSurface = 1
      fromEnum OpenGL    = 2
      fromEnum ASyncBlit = 4
      fromEnum OpenGLBlit = 10
      fromEnum Resizable = 16
      fromEnum NoFrame = 32
      fromEnum HWAccel = 256
      fromEnum SrcColorKey = 4096
      fromEnum RLEAccel = 16384
      fromEnum SrcAlpha = 65536
      fromEnum PreAlloc = 16777216
      fromEnum AnyFormat = 268435456
      fromEnum HWPalette = 536870912
      fromEnum DoubleBuf = 1073741824
      fromEnum Fullscreen = 2147483648
      toEnum 0 = SWSurface
      toEnum 1 = HWSurface
      toEnum 4 = ASyncBlit
      toEnum 2 = OpenGL
      toEnum 10 = OpenGLBlit
      toEnum 16 = Resizable
      toEnum 32 = NoFrame
      toEnum 256 = HWAccel
      toEnum 4096 = SrcColorKey
      toEnum 16384 = RLEAccel
      toEnum 65536 = SrcAlpha
      toEnum 16777216 = PreAlloc
      toEnum 268435456 = AnyFormat
      toEnum 536870912 = HWPalette
      toEnum 1073741824 = DoubleBuf
      toEnum 2147483648 = Fullscreen
      toEnum _ = error "Graphics.UI.SDL.Types.fromEnum: bad argument"
      succ SWSurface = HWSurface
      succ HWSurface = OpenGL
      succ OpenGL = ASyncBlit
      succ ASyncBlit = OpenGLBlit
      succ OpenGLBlit = Resizable
      succ Resizable = NoFrame
      succ NoFrame = HWAccel
      succ HWAccel = SrcColorKey
      succ SrcColorKey = RLEAccel
      succ RLEAccel = SrcAlpha
      succ SrcAlpha = PreAlloc
      succ PreAlloc = AnyFormat
      succ AnyFormat = HWPalette
      succ HWPalette = DoubleBuf
      succ DoubleBuf = Fullscreen
      succ _ = error "Graphics.UI.SDL.Types.succ: bad argument"

      pred HWSurface = SWSurface
      pred OpenGL = HWSurface
      pred ASyncBlit = OpenGL
      pred OpenGLBlit = ASyncBlit
      pred Resizable = OpenGLBlit
      pred NoFrame = Resizable
      pred HWAccel = NoFrame
      pred SrcColorKey = HWAccel
      pred RLEAccel = SrcColorKey
      pred SrcAlpha = RLEAccel
      pred PreAlloc = SrcAlpha
      pred AnyFormat = PreAlloc
      pred HWPalette = AnyFormat
      pred DoubleBuf = HWPalette
      pred Fullscreen = DoubleBuf
      pred _ = error "Graphics.UI.SDL.Types.pred: bad argument"

      enumFromTo x y | x > y = []
                     | x == y = [y]
                     | True = x : enumFromTo (succ x) y


surfaceGetPixelFormat :: Surface -> PixelFormat
surfaceGetPixelFormat surface
    = unsafePerformIO $
      withForeignPtr surface $ \ptr ->
      newForeignPtr_ =<< (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 232 "Graphics/UI/SDL/Types.hsc" #-}

pixelFormatGetAlpha :: PixelFormat -> IO Word8
pixelFormatGetAlpha format =
    withForeignPtr format $
    (\hsc_ptr -> peekByteOff hsc_ptr 40)
{-# LINE 237 "Graphics/UI/SDL/Types.hsc" #-}

pixelFormatGetColorKey :: PixelFormat -> IO Pixel
pixelFormatGetColorKey format =
    fmap Pixel $
    withForeignPtr format $
    (\hsc_ptr -> peekByteOff hsc_ptr 36)
{-# LINE 243 "Graphics/UI/SDL/Types.hsc" #-}

pixelFormatGetBitsPerPixel :: PixelFormat -> IO Word8
pixelFormatGetBitsPerPixel format
    = withForeignPtr format $
      (\hsc_ptr -> peekByteOff hsc_ptr 8)
{-# LINE 248 "Graphics/UI/SDL/Types.hsc" #-}

pixelFormatGetBytesPerPixel :: PixelFormat -> IO Word8
pixelFormatGetBytesPerPixel format
    = withForeignPtr format $
      (\hsc_ptr -> peekByteOff hsc_ptr 9)
{-# LINE 253 "Graphics/UI/SDL/Types.hsc" #-}

cintToInt :: CInt -> Int
cintToInt = fromIntegral

surfaceGetWidth :: Surface -> Int
surfaceGetWidth surface
    = cintToInt $ unsafePerformIO $
      withForeignPtr surface $
      (\hsc_ptr -> peekByteOff hsc_ptr 16)
{-# LINE 262 "Graphics/UI/SDL/Types.hsc" #-}

surfaceGetHeight :: Surface -> Int
surfaceGetHeight surface
    = cintToInt $ unsafePerformIO $
      withForeignPtr surface $
      (\hsc_ptr -> peekByteOff hsc_ptr 20)
{-# LINE 268 "Graphics/UI/SDL/Types.hsc" #-}

surfaceGetFlags :: Surface -> IO [SurfaceFlag]
surfaceGetFlags surface
    = withForeignPtr surface $
      fmap fromBitmask . (\hsc_ptr -> peekByteOff hsc_ptr 0)
{-# LINE 273 "Graphics/UI/SDL/Types.hsc" #-}

surfaceGetPitch :: Surface -> Word16
surfaceGetPitch surface
    = unsafePerformIO $
      withForeignPtr surface $
      (\hsc_ptr -> peekByteOff hsc_ptr 24)
{-# LINE 279 "Graphics/UI/SDL/Types.hsc" #-}

surfaceGetPixels :: Surface -> IO Pixels
surfaceGetPixels surface
    = withForeignPtr surface $
      (\hsc_ptr -> peekByteOff hsc_ptr 32)
{-# LINE 284 "Graphics/UI/SDL/Types.hsc" #-}

videoInfoWidth :: VideoInfo -> Int
videoInfoWidth vi
    = cintToInt $ unsafePerformIO $
      withForeignPtr vi $
      (\hsc_ptr -> peekByteOff hsc_ptr 16)
{-# LINE 290 "Graphics/UI/SDL/Types.hsc" #-}

videoInfoHeight :: VideoInfo -> Int
videoInfoHeight vi
    = cintToInt $ unsafePerformIO $
      withForeignPtr vi $
      (\hsc_ptr -> peekByteOff hsc_ptr 20)
{-# LINE 296 "Graphics/UI/SDL/Types.hsc" #-}

