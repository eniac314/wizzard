{-# LINE 1 "Graphics/UI/SDL/Audio.hsc" #-}

{-# LINE 2 "Graphics/UI/SDL/Audio.hsc" #-}

{-# LINE 5 "Graphics/UI/SDL/Audio.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Audio
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Audio
    ( AudioFormat (..)
    , fromAudioFormat
    , toAudioFormat
    ) where

import Data.Word (Word16)

data AudioFormat
    = AudioU8
    | AudioS8
    | AudioU16LSB
    | AudioS16LSB
    | AudioU16MSB
    | AudioS16MSB
    | AudioU16Sys
    | AudioS16Sys
      deriving (Show,Eq,Ord,Enum)

fromAudioFormat :: AudioFormat -> Word16
fromAudioFormat AudioU8 = 8
{-# LINE 37 "Graphics/UI/SDL/Audio.hsc" #-}
fromAudioFormat AudioS8 = 32776
{-# LINE 38 "Graphics/UI/SDL/Audio.hsc" #-}
fromAudioFormat AudioU16LSB = 16
{-# LINE 39 "Graphics/UI/SDL/Audio.hsc" #-}
fromAudioFormat AudioS16LSB = 32784
{-# LINE 40 "Graphics/UI/SDL/Audio.hsc" #-}
fromAudioFormat AudioU16MSB = 4112
{-# LINE 41 "Graphics/UI/SDL/Audio.hsc" #-}
fromAudioFormat AudioS16MSB = 36880
{-# LINE 42 "Graphics/UI/SDL/Audio.hsc" #-}
fromAudioFormat AudioU16Sys = 16
{-# LINE 43 "Graphics/UI/SDL/Audio.hsc" #-}
fromAudioFormat AudioS16Sys = 32784
{-# LINE 44 "Graphics/UI/SDL/Audio.hsc" #-}

toAudioFormat :: Word16 -> AudioFormat
toAudioFormat 8 = AudioU8
{-# LINE 47 "Graphics/UI/SDL/Audio.hsc" #-}
toAudioFormat 32776 = AudioS8
{-# LINE 48 "Graphics/UI/SDL/Audio.hsc" #-}
toAudioFormat 16 = AudioU16LSB
{-# LINE 49 "Graphics/UI/SDL/Audio.hsc" #-}
toAudioFormat 32784 = AudioS16LSB
{-# LINE 50 "Graphics/UI/SDL/Audio.hsc" #-}
toAudioFormat 4112 = AudioU16MSB
{-# LINE 51 "Graphics/UI/SDL/Audio.hsc" #-}
toAudioFormat 36880 = AudioS16MSB
{-# LINE 52 "Graphics/UI/SDL/Audio.hsc" #-}
toAudioFormat _ = error "Graphics.UI.SDL.Audio.toAudioFormat: bad argument"


