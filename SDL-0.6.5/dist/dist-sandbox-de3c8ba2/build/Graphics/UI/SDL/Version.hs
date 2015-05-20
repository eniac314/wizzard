{-# LINE 1 "Graphics/UI/SDL/Version.hsc" #-}

{-# LINE 2 "Graphics/UI/SDL/Version.hsc" #-}

{-# LINE 5 "Graphics/UI/SDL/Version.hsc" #-}
module Graphics.UI.SDL.Version
    ( compiledFor
    , linkedWith
    ) where

import Data.Version (Version(Version))

import Foreign (Word8, Ptr, Storable(sizeOf, alignment, peekByteOff, peek))

data SDLVersion
    = SDLVersion Word8 Word8 Word8

instance Storable SDLVersion where
    sizeOf _ = (3)
{-# LINE 19 "Graphics/UI/SDL/Version.hsc" #-}
    alignment _ = 1
    peek ptr = do major <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 21 "Graphics/UI/SDL/Version.hsc" #-}
                  minor <- (\hsc_ptr -> peekByteOff hsc_ptr 1) ptr
{-# LINE 22 "Graphics/UI/SDL/Version.hsc" #-}
                  patch <- (\hsc_ptr -> peekByteOff hsc_ptr 2) ptr
{-# LINE 23 "Graphics/UI/SDL/Version.hsc" #-}
                  return (SDLVersion major minor patch)

compiledFor :: Version
compiledFor = Version [ 1
{-# LINE 27 "Graphics/UI/SDL/Version.hsc" #-}
                      , 2
{-# LINE 28 "Graphics/UI/SDL/Version.hsc" #-}
                      , 15
{-# LINE 29 "Graphics/UI/SDL/Version.hsc" #-}
                      ] []

-- const SDL_version * SDL_Linked_Version(void);
foreign import ccall unsafe "SDL_Linked_Version" sdlLinkedVersion :: IO (Ptr SDLVersion)
linkedWith :: IO Version
linkedWith = do versionPtr <- sdlLinkedVersion
                SDLVersion major minor patch <- peek versionPtr
                return (Version (map fromIntegral [major,minor,patch]) [])
