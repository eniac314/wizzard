{-# LINE 1 "Graphics/UI/SDL/Rect.hsc" #-}

{-# LINE 2 "Graphics/UI/SDL/Rect.hsc" #-}

{-# LINE 5 "Graphics/UI/SDL/Rect.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Video
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Rect where

import Foreign (Storable(poke, sizeOf, alignment, peekByteOff, pokeByteOff,
                         peek))
import Data.Word (Word16)
import Data.Int (Int16)

data Rect
    = Rect
    { rectX, rectY :: Int,  -- Actually Int16
      rectW, rectH :: Int } -- Actually Word16
    deriving (Show,Eq,Ord)

instance Storable Rect where
    sizeOf = const (8)
{-# LINE 31 "Graphics/UI/SDL/Rect.hsc" #-}
    alignment = const 2
    peek ptr
        = do x <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr :: IO Int16
{-# LINE 34 "Graphics/UI/SDL/Rect.hsc" #-}
             y <- (\hsc_ptr -> peekByteOff hsc_ptr 2) ptr :: IO Int16
{-# LINE 35 "Graphics/UI/SDL/Rect.hsc" #-}
             w <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr :: IO Word16
{-# LINE 36 "Graphics/UI/SDL/Rect.hsc" #-}
             h <- (\hsc_ptr -> peekByteOff hsc_ptr 6) ptr :: IO Word16
{-# LINE 37 "Graphics/UI/SDL/Rect.hsc" #-}
             return $! Rect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
    poke ptr (Rect x y w h)
        = do (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr (fromIntegral x :: Int16)
{-# LINE 40 "Graphics/UI/SDL/Rect.hsc" #-}
             (\hsc_ptr -> pokeByteOff hsc_ptr 2) ptr (fromIntegral y :: Int16)
{-# LINE 41 "Graphics/UI/SDL/Rect.hsc" #-}
             (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr (fromIntegral w :: Word16)
{-# LINE 42 "Graphics/UI/SDL/Rect.hsc" #-}
             (\hsc_ptr -> pokeByteOff hsc_ptr 6) ptr (fromIntegral h :: Word16)
{-# LINE 43 "Graphics/UI/SDL/Rect.hsc" #-}

