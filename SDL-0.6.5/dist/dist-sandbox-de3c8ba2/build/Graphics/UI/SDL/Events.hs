{-# LINE 1 "Graphics/UI/SDL/Events.hsc" #-}

{-# LINE 2 "Graphics/UI/SDL/Events.hsc" #-}

{-# LINE 5 "Graphics/UI/SDL/Events.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Events
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Events
    ( Event (..)
    , SDLEvent (..)
    , UserEventID (..)
    , MouseButton (..)
    , Focus(..)
    , toSafePtr
    , tryFromSafePtr
    , fromSafePtr
    , typeOfSafePtr
    , enableKeyRepeat
    , enableUnicode
    , queryUnicodeState
    , getKeyName
    , getMouseState
    , getRelativeMouseState
    , getModState
    , setModState
    , tryPushEvent
    , pushEvent
    , pollEvent
    , waitEvent
    , waitEventBlocking
    , pumpEvents
    , enableEvent
    , queryEventState
    , getAppState
    ) where

import Foreign (Int16, Word8, Word16, Word32, Ptr,
               Storable(poke, sizeOf, alignment, peekByteOff, pokeByteOff, peek),
               toBool, new, alloca)
import Foreign.C (peekCString, CString, CInt)
import System.IO.Unsafe (unsafePerformIO)
import Data.Bits (Bits((.&.), shiftL))
import Control.Concurrent (threadDelay)
import Prelude hiding (Enum(..))
import qualified Prelude (Enum(..))

import Foreign.StablePtr (newStablePtr,castStablePtrToPtr,castPtrToStablePtr,deRefStablePtr)
import Data.Typeable
import Graphics.UI.SDL.Keysym (SDLKey, Modifier, Keysym)
import Graphics.UI.SDL.Utilities (Enum(..), intToBool, toBitmask, fromBitmask, fromCInt)
import Graphics.UI.SDL.General (unwrapBool, failWithError)
import Graphics.UI.SDL.Video (Toggle(..), fromToggle)

-- |Low level event structure keeping a one-to-one relation with the C event structure.
data SDLEvent = SDLNoEvent
              | SDLActiveEvent
              | SDLKeyDown
              | SDLKeyUp
              | SDLMouseMotion
              | SDLMouseButtonDown
              | SDLMouseButtonUp
              | SDLJoyAxisMotion
              | SDLJoyBallMotion
              | SDLJoyHatMotion
              | SDLJoyButtonDown
              | SDLJoyButtonUp
              | SDLQuit
              | SDLSysWMEvent
              | SDLVideoResize
              | SDLVideoExpose
              | SDLUserEvent Word8
              | SDLNumEvents
    deriving (Eq, Ord, Show)
instance Bounded SDLEvent where
    minBound = SDLNoEvent
    maxBound = SDLNumEvents

fromSDLEvent :: SDLEvent -> Word8
fromSDLEvent SDLNoEvent = 0
{-# LINE 88 "Graphics/UI/SDL/Events.hsc" #-}
fromSDLEvent SDLActiveEvent = 1
{-# LINE 89 "Graphics/UI/SDL/Events.hsc" #-}
fromSDLEvent SDLKeyDown = 2
{-# LINE 90 "Graphics/UI/SDL/Events.hsc" #-}
fromSDLEvent SDLKeyUp = 3
{-# LINE 91 "Graphics/UI/SDL/Events.hsc" #-}
fromSDLEvent SDLMouseMotion = 4
{-# LINE 92 "Graphics/UI/SDL/Events.hsc" #-}
fromSDLEvent SDLMouseButtonDown = 5
{-# LINE 93 "Graphics/UI/SDL/Events.hsc" #-}
fromSDLEvent SDLMouseButtonUp = 6
{-# LINE 94 "Graphics/UI/SDL/Events.hsc" #-}
fromSDLEvent SDLJoyAxisMotion = 7
{-# LINE 95 "Graphics/UI/SDL/Events.hsc" #-}
fromSDLEvent SDLJoyBallMotion = 8
{-# LINE 96 "Graphics/UI/SDL/Events.hsc" #-}
fromSDLEvent SDLJoyHatMotion = 9
{-# LINE 97 "Graphics/UI/SDL/Events.hsc" #-}
fromSDLEvent SDLJoyButtonDown = 10
{-# LINE 98 "Graphics/UI/SDL/Events.hsc" #-}
fromSDLEvent SDLJoyButtonUp = 11
{-# LINE 99 "Graphics/UI/SDL/Events.hsc" #-}
fromSDLEvent SDLQuit = 12
{-# LINE 100 "Graphics/UI/SDL/Events.hsc" #-}
fromSDLEvent SDLSysWMEvent = 13
{-# LINE 101 "Graphics/UI/SDL/Events.hsc" #-}
fromSDLEvent SDLVideoResize = 16
{-# LINE 102 "Graphics/UI/SDL/Events.hsc" #-}
fromSDLEvent SDLVideoExpose = 17
{-# LINE 103 "Graphics/UI/SDL/Events.hsc" #-}
fromSDLEvent (SDLUserEvent n) = 24 + n
{-# LINE 104 "Graphics/UI/SDL/Events.hsc" #-}
fromSDLEvent SDLNumEvents = 32
{-# LINE 105 "Graphics/UI/SDL/Events.hsc" #-}

toSDLEvent :: Word8 -> SDLEvent
toSDLEvent 0 = SDLNoEvent
{-# LINE 108 "Graphics/UI/SDL/Events.hsc" #-}
toSDLEvent 1 = SDLActiveEvent
{-# LINE 109 "Graphics/UI/SDL/Events.hsc" #-}
toSDLEvent 2 = SDLKeyDown
{-# LINE 110 "Graphics/UI/SDL/Events.hsc" #-}
toSDLEvent 3 = SDLKeyUp
{-# LINE 111 "Graphics/UI/SDL/Events.hsc" #-}
toSDLEvent 4 = SDLMouseMotion
{-# LINE 112 "Graphics/UI/SDL/Events.hsc" #-}
toSDLEvent 5 = SDLMouseButtonDown
{-# LINE 113 "Graphics/UI/SDL/Events.hsc" #-}
toSDLEvent 6 = SDLMouseButtonUp
{-# LINE 114 "Graphics/UI/SDL/Events.hsc" #-}
toSDLEvent 7 = SDLJoyAxisMotion
{-# LINE 115 "Graphics/UI/SDL/Events.hsc" #-}
toSDLEvent 8 = SDLJoyBallMotion
{-# LINE 116 "Graphics/UI/SDL/Events.hsc" #-}
toSDLEvent 9 = SDLJoyHatMotion
{-# LINE 117 "Graphics/UI/SDL/Events.hsc" #-}
toSDLEvent 10 = SDLJoyButtonDown
{-# LINE 118 "Graphics/UI/SDL/Events.hsc" #-}
toSDLEvent 11 = SDLJoyButtonUp
{-# LINE 119 "Graphics/UI/SDL/Events.hsc" #-}
toSDLEvent 12 = SDLQuit
{-# LINE 120 "Graphics/UI/SDL/Events.hsc" #-}
toSDLEvent 13 = SDLSysWMEvent
{-# LINE 121 "Graphics/UI/SDL/Events.hsc" #-}
toSDLEvent 16 = SDLVideoResize
{-# LINE 122 "Graphics/UI/SDL/Events.hsc" #-}
toSDLEvent 17 = SDLVideoExpose
{-# LINE 123 "Graphics/UI/SDL/Events.hsc" #-}
toSDLEvent n 
    | n >= 24 &&
{-# LINE 125 "Graphics/UI/SDL/Events.hsc" #-}
      n <  32 = SDLUserEvent (n - 24)
{-# LINE 126 "Graphics/UI/SDL/Events.hsc" #-}
toSDLEvent _ = error "Graphics.UI.SDL.Events.toSDLEvent: bad argument"

-- |High level event structure.
data Event
    = NoEvent
    | GotFocus [Focus]
    | LostFocus [Focus]
    | KeyDown !Keysym
    | KeyUp !Keysym
    | MouseMotion !Word16 !Word16 !Int16 !Int16
    | MouseButtonDown !Word16
                      !Word16
                      !MouseButton
    | MouseButtonUp !Word16
                    !Word16
                    !MouseButton
    | JoyAxisMotion !Word8 !Word8 !Int16
      -- ^ device index, axis index, axis value.
    | JoyBallMotion !Word8 !Word8 !Int16 !Int16
      -- ^ device index, trackball index, relative motion.
    | JoyHatMotion !Word8 !Word8 !Word8
      -- ^ device index, hat index, hat position.
    | JoyButtonDown !Word8 !Word8
      -- ^ device index, button index.
    | JoyButtonUp !Word8 !Word8
      -- ^ device index, button index.
    | VideoResize !Int !Int
      -- ^ When @Resizable@ is passed as a flag to 'Graphics.UI.SDL.Video.setVideoMode' the user is
      --   allowed to resize the applications window. When the window is resized
      --   an @VideoResize@ is reported, with the new window width and height values.
      --   When an @VideoResize@ is recieved the window should be resized to the
      --   new dimensions using 'Graphics.UI.SDL.Video.setVideoMode'.
    | VideoExpose
      -- ^ A @VideoExpose@ event is triggered when the screen has been modified
      --   outside of the application, usually by the window manager and needs to be redrawn.
    | Quit
    | User !UserEventID !Int !(Ptr ()) !(Ptr ())
    | Unknown
      deriving (Show,Eq)

data MouseButton
    = ButtonLeft
    | ButtonMiddle
    | ButtonRight
    | ButtonWheelUp
    | ButtonWheelDown
    | ButtonUnknown !Word8
      deriving (Show,Eq,Ord)

instance Enum MouseButton Word8 where
    toEnum 1 = ButtonLeft
{-# LINE 177 "Graphics/UI/SDL/Events.hsc" #-}
    toEnum 2 = ButtonMiddle
{-# LINE 178 "Graphics/UI/SDL/Events.hsc" #-}
    toEnum 3 = ButtonRight
{-# LINE 179 "Graphics/UI/SDL/Events.hsc" #-}
    toEnum 4 = ButtonWheelUp
{-# LINE 180 "Graphics/UI/SDL/Events.hsc" #-}
    toEnum 5 = ButtonWheelDown
{-# LINE 181 "Graphics/UI/SDL/Events.hsc" #-}
    toEnum n = ButtonUnknown (fromIntegral n)
    fromEnum ButtonLeft = 1
{-# LINE 183 "Graphics/UI/SDL/Events.hsc" #-}
    fromEnum ButtonMiddle = 2
{-# LINE 184 "Graphics/UI/SDL/Events.hsc" #-}
    fromEnum ButtonRight = 3
{-# LINE 185 "Graphics/UI/SDL/Events.hsc" #-}
    fromEnum ButtonWheelUp = 4
{-# LINE 186 "Graphics/UI/SDL/Events.hsc" #-}
    fromEnum ButtonWheelDown = 5
{-# LINE 187 "Graphics/UI/SDL/Events.hsc" #-}
    fromEnum (ButtonUnknown n) = fromIntegral n
    succ = toEnum . (+1) . fromEnum
    pred = toEnum . (subtract 1) . fromEnum
    enumFromTo = defEnumFromTo

mouseButtonMask :: MouseButton -> Word8
mouseButtonMask ButtonLeft = 1
{-# LINE 194 "Graphics/UI/SDL/Events.hsc" #-}
mouseButtonMask ButtonMiddle = 2
{-# LINE 195 "Graphics/UI/SDL/Events.hsc" #-}
mouseButtonMask ButtonRight = 4
{-# LINE 196 "Graphics/UI/SDL/Events.hsc" #-}
mouseButtonMask ButtonWheelUp = 8
{-# LINE 197 "Graphics/UI/SDL/Events.hsc" #-}
mouseButtonMask ButtonWheelDown = 16
{-# LINE 198 "Graphics/UI/SDL/Events.hsc" #-}
mouseButtonMask (ButtonUnknown n) = 1 `shiftL` (fromIntegral n-1)

allButtons :: [MouseButton]
allButtons = [ButtonLeft
             ,ButtonMiddle
             ,ButtonRight
             ,ButtonWheelUp
             ,ButtonWheelDown
             ]

defEnumFromTo :: (Enum a b, Ord a) => a -> a -> [a]
defEnumFromTo x y | x > y     = []
                  | x == y    = [y]
                  | otherwise = x : defEnumFromTo (succ x) y


data Focus
    = MouseFocus
    | InputFocus
    | ApplicationFocus
      deriving (Show,Eq,Ord)

instance Bounded Focus where
    minBound = MouseFocus
    maxBound = ApplicationFocus

instance Enum Focus Word8 where
    fromEnum MouseFocus = 1
{-# LINE 226 "Graphics/UI/SDL/Events.hsc" #-}
    fromEnum InputFocus = 2
{-# LINE 227 "Graphics/UI/SDL/Events.hsc" #-}
    fromEnum ApplicationFocus = 4
{-# LINE 228 "Graphics/UI/SDL/Events.hsc" #-}
    toEnum 1 = MouseFocus
{-# LINE 229 "Graphics/UI/SDL/Events.hsc" #-}
    toEnum 2 = InputFocus
{-# LINE 230 "Graphics/UI/SDL/Events.hsc" #-}
    toEnum 4 = ApplicationFocus
{-# LINE 231 "Graphics/UI/SDL/Events.hsc" #-}
    toEnum _ = error "Graphics.UI.SDL.Events.toEnum: bad argument"
    succ MouseFocus = InputFocus
    succ InputFocus = ApplicationFocus
    succ _ = error "Graphics.UI.SDL.Events.succ: bad argument"
    pred InputFocus = MouseFocus
    pred ApplicationFocus = InputFocus
    pred _ = error "Graphics.UI.SDL.Events.pred: bad argument"
    enumFromTo x y | x > y = []
                   | x == y = [y]
                   | True = x : enumFromTo (succ x) y

-- |Typed user events ranging from 0 to 7
data UserEventID
    = UID0 | UID1 | UID2 | UID3 | UID4 | UID5 | UID6 | UID7
      deriving (Show,Eq,Prelude.Enum)

-- |A safe pointer keeps the type of the object it was created from
--  and checks it when it's deconstructed.
type SafePtr = Ptr ()

-- |Constructs a safe pointer from an arbitrary value.
toSafePtr :: (Typeable a) => a -> IO SafePtr
toSafePtr val
    = do stablePtr <- newStablePtr (typeOf val,val)
         return (castStablePtrToPtr stablePtr)

-- |Return the type of the object the safe pointer was created from.
typeOfSafePtr :: SafePtr -> IO TypeRep
typeOfSafePtr ptr
    = fmap fst (deRefStablePtr (castPtrToStablePtr ptr))

-- |Get object from a safe pointer. @Nothing@ on type mismatch.
tryFromSafePtr :: (Typeable a) => SafePtr -> IO (Maybe a)
tryFromSafePtr ptr
    = do (ty,val) <- deRefStablePtr (castPtrToStablePtr ptr)
         if ty == typeOf val
            then return (Just val)
            else return Nothing

-- |Get object from a safe pointer. Throws an exception on type mismatch.
fromSafePtr :: (Typeable a) => SafePtr -> IO a
fromSafePtr ptr
    = do ret <- tryFromSafePtr ptr
         case ret of
           Nothing -> error "Graphics.UI.SDL.Events.fromSafePtr: invalid type."
           Just a  -> return a

toEventType :: UserEventID -> Word8
toEventType eid = fromIntegral (Prelude.fromEnum eid)

fromEventType :: Word8 -> UserEventID
fromEventType etype = Prelude.toEnum (fromIntegral etype)

peekActiveEvent :: Ptr Event -> IO Event
peekActiveEvent ptr
    = do gain <- fmap toBool (((\hsc_ptr -> peekByteOff hsc_ptr 1) ptr) :: IO Word8)
{-# LINE 287 "Graphics/UI/SDL/Events.hsc" #-}
         state <- (\hsc_ptr -> peekByteOff hsc_ptr 2) ptr :: IO Word8
{-# LINE 288 "Graphics/UI/SDL/Events.hsc" #-}
         return $! (if gain then GotFocus else LostFocus) (fromBitmask state)

peekKey :: (Keysym -> Event) -> Ptr Event -> IO Event
peekKey mkEvent ptr
    = do keysym <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 293 "Graphics/UI/SDL/Events.hsc" #-}
         return $! mkEvent keysym

peekMouseMotion :: Ptr Event -> IO Event
peekMouseMotion ptr
    = do x <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 298 "Graphics/UI/SDL/Events.hsc" #-}
         y <- (\hsc_ptr -> peekByteOff hsc_ptr 6) ptr
{-# LINE 299 "Graphics/UI/SDL/Events.hsc" #-}
         xrel <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 300 "Graphics/UI/SDL/Events.hsc" #-}
         yrel <- (\hsc_ptr -> peekByteOff hsc_ptr 10) ptr
{-# LINE 301 "Graphics/UI/SDL/Events.hsc" #-}
         return $! MouseMotion x y xrel yrel

peekMouse :: (Word16 -> Word16 -> MouseButton -> Event) -> Ptr Event -> IO Event
peekMouse mkEvent ptr
    = do b <- (\hsc_ptr -> peekByteOff hsc_ptr 2) ptr
{-# LINE 306 "Graphics/UI/SDL/Events.hsc" #-}
         x <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 307 "Graphics/UI/SDL/Events.hsc" #-}
         y <- (\hsc_ptr -> peekByteOff hsc_ptr 6) ptr
{-# LINE 308 "Graphics/UI/SDL/Events.hsc" #-}
         return $! mkEvent x y (toEnum (b::Word8))

peekJoyAxisMotion :: Ptr Event -> IO Event
peekJoyAxisMotion ptr
    = do which <- (\hsc_ptr -> peekByteOff hsc_ptr 1) ptr
{-# LINE 313 "Graphics/UI/SDL/Events.hsc" #-}
         axis <- (\hsc_ptr -> peekByteOff hsc_ptr 2) ptr
{-# LINE 314 "Graphics/UI/SDL/Events.hsc" #-}
         value <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 315 "Graphics/UI/SDL/Events.hsc" #-}
         return $! JoyAxisMotion which axis value

peekJoyBallMotion :: Ptr Event -> IO Event
peekJoyBallMotion ptr
    = do which <- (\hsc_ptr -> peekByteOff hsc_ptr 1) ptr
{-# LINE 320 "Graphics/UI/SDL/Events.hsc" #-}
         ball <- (\hsc_ptr -> peekByteOff hsc_ptr 2) ptr
{-# LINE 321 "Graphics/UI/SDL/Events.hsc" #-}
         xrel <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 322 "Graphics/UI/SDL/Events.hsc" #-}
         yrel <- (\hsc_ptr -> peekByteOff hsc_ptr 6) ptr
{-# LINE 323 "Graphics/UI/SDL/Events.hsc" #-}
         return $! JoyBallMotion which ball xrel yrel

peekJoyHatMotion :: Ptr Event -> IO Event
peekJoyHatMotion ptr
    = do which <- (\hsc_ptr -> peekByteOff hsc_ptr 1) ptr
{-# LINE 328 "Graphics/UI/SDL/Events.hsc" #-}
         hat <- (\hsc_ptr -> peekByteOff hsc_ptr 2) ptr
{-# LINE 329 "Graphics/UI/SDL/Events.hsc" #-}
         value <- (\hsc_ptr -> peekByteOff hsc_ptr 3) ptr
{-# LINE 330 "Graphics/UI/SDL/Events.hsc" #-}
         return $! JoyHatMotion which hat value

peekJoyButton :: (Word8 -> Word8 -> Event) -> Ptr Event -> IO Event
peekJoyButton mkEvent ptr
    = do which <- (\hsc_ptr -> peekByteOff hsc_ptr 1) ptr
{-# LINE 335 "Graphics/UI/SDL/Events.hsc" #-}
         button <- (\hsc_ptr -> peekByteOff hsc_ptr 2) ptr
{-# LINE 336 "Graphics/UI/SDL/Events.hsc" #-}
         return $! mkEvent which button

peekResize :: Ptr Event -> IO Event
peekResize ptr
    = do w <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 341 "Graphics/UI/SDL/Events.hsc" #-}
         h <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 342 "Graphics/UI/SDL/Events.hsc" #-}
         return $! VideoResize (fromCInt w) (fromCInt h)

peekUserEvent :: Ptr Event -> Word8 -> IO Event
peekUserEvent ptr n
    = do code <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 347 "Graphics/UI/SDL/Events.hsc" #-}
         data1 <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 348 "Graphics/UI/SDL/Events.hsc" #-}
         data2 <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 349 "Graphics/UI/SDL/Events.hsc" #-}
         return $ User (fromEventType n) (fromCInt code) data1 data2

getEventType :: Event -> Word8
getEventType = fromSDLEvent . eventToSDLEvent

eventToSDLEvent :: Event -> SDLEvent
eventToSDLEvent NoEvent = SDLNoEvent
eventToSDLEvent (GotFocus _) = SDLActiveEvent
eventToSDLEvent (LostFocus _) = SDLActiveEvent
eventToSDLEvent (KeyDown _) = SDLKeyDown
eventToSDLEvent (KeyUp _) = SDLKeyUp
eventToSDLEvent (MouseMotion _ _ _ _) = SDLMouseMotion
eventToSDLEvent (MouseButtonDown _ _ _) = SDLMouseButtonDown
eventToSDLEvent (MouseButtonUp _ _ _) = SDLMouseButtonUp
eventToSDLEvent (JoyAxisMotion _ _ _) = SDLJoyAxisMotion
eventToSDLEvent (JoyBallMotion _ _ _ _) = SDLJoyBallMotion
eventToSDLEvent (JoyHatMotion _ _ _) = SDLJoyHatMotion
eventToSDLEvent (JoyButtonDown _ _) = SDLJoyButtonDown
eventToSDLEvent (JoyButtonUp _ _) = SDLJoyButtonUp
eventToSDLEvent Quit = SDLQuit
eventToSDLEvent (VideoResize _ _) = SDLVideoResize
eventToSDLEvent VideoExpose = SDLVideoExpose
eventToSDLEvent (User uid _ _ _) = SDLUserEvent (toEventType uid)
eventToSDLEvent _ = error "Graphics.UI.SDL.Events.eventToSDLEvent: bad argument"

pokeActiveEvent :: Ptr Event -> Word8 -> [Focus] -> IO ()
pokeActiveEvent ptr gain focus
    = do (\hsc_ptr -> pokeByteOff hsc_ptr 1) ptr gain
{-# LINE 377 "Graphics/UI/SDL/Events.hsc" #-}
         (\hsc_ptr -> pokeByteOff hsc_ptr 2) ptr (toBitmask focus)
{-# LINE 378 "Graphics/UI/SDL/Events.hsc" #-}

pokeKey :: Ptr Event -> Word8 -> Keysym -> IO ()
pokeKey ptr state keysym
    = do (\hsc_ptr -> pokeByteOff hsc_ptr 2) ptr state
{-# LINE 382 "Graphics/UI/SDL/Events.hsc" #-}
         (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr keysym
{-# LINE 383 "Graphics/UI/SDL/Events.hsc" #-}

pokeMouseMotion :: Ptr Event -> Word16 -> Word16 -> Int16 -> Int16 -> IO ()
pokeMouseMotion ptr x y xrel yrel
    = do (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr x
{-# LINE 387 "Graphics/UI/SDL/Events.hsc" #-}
         (\hsc_ptr -> pokeByteOff hsc_ptr 6) ptr y
{-# LINE 388 "Graphics/UI/SDL/Events.hsc" #-}
         (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr xrel
{-# LINE 389 "Graphics/UI/SDL/Events.hsc" #-}
         (\hsc_ptr -> pokeByteOff hsc_ptr 10) ptr yrel
{-# LINE 390 "Graphics/UI/SDL/Events.hsc" #-}

pokeMouseButton :: Ptr Event -> Word8 -> Word16 -> Word16 -> MouseButton -> IO ()
pokeMouseButton ptr state x y b
    = do (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr x
{-# LINE 394 "Graphics/UI/SDL/Events.hsc" #-}
         (\hsc_ptr -> pokeByteOff hsc_ptr 6) ptr y
{-# LINE 395 "Graphics/UI/SDL/Events.hsc" #-}
         (\hsc_ptr -> pokeByteOff hsc_ptr 3) ptr state
{-# LINE 396 "Graphics/UI/SDL/Events.hsc" #-}
         (\hsc_ptr -> pokeByteOff hsc_ptr 2) ptr (fromEnum b)
{-# LINE 397 "Graphics/UI/SDL/Events.hsc" #-}

pokeJoyAxisMotion :: Ptr Event -> Word8 -> Word8 -> Int16 -> IO ()
pokeJoyAxisMotion ptr which axis value
    = do (\hsc_ptr -> pokeByteOff hsc_ptr 1) ptr which
{-# LINE 401 "Graphics/UI/SDL/Events.hsc" #-}
         (\hsc_ptr -> pokeByteOff hsc_ptr 2) ptr axis
{-# LINE 402 "Graphics/UI/SDL/Events.hsc" #-}
         (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr value
{-# LINE 403 "Graphics/UI/SDL/Events.hsc" #-}

pokeJoyBallMotion :: Ptr Event -> Word8 -> Word8 -> Int16 -> Int16 -> IO ()
pokeJoyBallMotion ptr which ball xrel yrel
    = do (\hsc_ptr -> pokeByteOff hsc_ptr 1) ptr which
{-# LINE 407 "Graphics/UI/SDL/Events.hsc" #-}
         (\hsc_ptr -> pokeByteOff hsc_ptr 2) ptr ball
{-# LINE 408 "Graphics/UI/SDL/Events.hsc" #-}
         (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr xrel
{-# LINE 409 "Graphics/UI/SDL/Events.hsc" #-}
         (\hsc_ptr -> pokeByteOff hsc_ptr 6) ptr yrel
{-# LINE 410 "Graphics/UI/SDL/Events.hsc" #-}

pokeJoyHatMotion :: Ptr Event -> Word8 -> Word8 -> Word8 -> IO ()
pokeJoyHatMotion ptr which hat value
    = do (\hsc_ptr -> pokeByteOff hsc_ptr 1) ptr which
{-# LINE 414 "Graphics/UI/SDL/Events.hsc" #-}
         (\hsc_ptr -> pokeByteOff hsc_ptr 2) ptr hat
{-# LINE 415 "Graphics/UI/SDL/Events.hsc" #-}
         (\hsc_ptr -> pokeByteOff hsc_ptr 3) ptr value
{-# LINE 416 "Graphics/UI/SDL/Events.hsc" #-}

pokeJoyButton :: Ptr Event -> Word8 -> Word8 -> Word8 -> IO ()
pokeJoyButton ptr which button state
    = do (\hsc_ptr -> pokeByteOff hsc_ptr 1) ptr which
{-# LINE 420 "Graphics/UI/SDL/Events.hsc" #-}
         (\hsc_ptr -> pokeByteOff hsc_ptr 2) ptr button
{-# LINE 421 "Graphics/UI/SDL/Events.hsc" #-}
         (\hsc_ptr -> pokeByteOff hsc_ptr 3) ptr state
{-# LINE 422 "Graphics/UI/SDL/Events.hsc" #-}

pokeResize :: Ptr Event -> Int -> Int -> IO ()
pokeResize ptr w h
    = do (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr w
{-# LINE 426 "Graphics/UI/SDL/Events.hsc" #-}
         (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr h
{-# LINE 427 "Graphics/UI/SDL/Events.hsc" #-}

pokeUserEvent :: Ptr Event -> UserEventID -> Int -> Ptr () -> Ptr () -> IO ()
pokeUserEvent ptr _eventId code data1 data2
    = do (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr code
{-# LINE 431 "Graphics/UI/SDL/Events.hsc" #-}
         (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr data1
{-# LINE 432 "Graphics/UI/SDL/Events.hsc" #-}
         (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr data2
{-# LINE 433 "Graphics/UI/SDL/Events.hsc" #-}

instance Storable Event where
    sizeOf = const ((24))
{-# LINE 436 "Graphics/UI/SDL/Events.hsc" #-}
    alignment = const 4
    poke ptr event
        = do pokeByteOff ptr 0 (getEventType event)
             case event of
               NoEvent               -> return ()
               GotFocus focus        -> pokeActiveEvent ptr 1 focus
               LostFocus focus       -> pokeActiveEvent ptr 0 focus
               KeyDown keysym        -> pokeKey ptr 1 keysym
{-# LINE 444 "Graphics/UI/SDL/Events.hsc" #-}
               KeyUp keysym          -> pokeKey ptr 0 keysym
{-# LINE 445 "Graphics/UI/SDL/Events.hsc" #-}
               MouseMotion x y xrel yrel -> pokeMouseMotion ptr x y xrel yrel
               MouseButtonDown x y b -> pokeMouseButton ptr 1 x y b
{-# LINE 447 "Graphics/UI/SDL/Events.hsc" #-}
               MouseButtonUp x y b   -> pokeMouseButton ptr 0 x y b
{-# LINE 448 "Graphics/UI/SDL/Events.hsc" #-}
               JoyAxisMotion w a v   -> pokeJoyAxisMotion ptr w a v
               JoyBallMotion w b x y -> pokeJoyBallMotion ptr w b x y
               JoyHatMotion w h v    -> pokeJoyHatMotion ptr w h v
               JoyButtonDown w b     -> pokeJoyButton ptr w b 1
{-# LINE 452 "Graphics/UI/SDL/Events.hsc" #-}
               JoyButtonUp w b       -> pokeJoyButton ptr w b 0
{-# LINE 453 "Graphics/UI/SDL/Events.hsc" #-}
               Quit                  -> return ()
               VideoResize w h       -> pokeResize ptr w h
               VideoExpose           -> return ()
               User eventId c d1 d2  -> pokeUserEvent ptr eventId c d1 d2
               e                     -> failWithError $ "Unhandled eventtype: " ++ show e
    peek ptr
        = do eventType <- peekByteOff ptr 0
             case toSDLEvent eventType of
               SDLNoEvent         -> return NoEvent
               SDLActiveEvent     -> peekActiveEvent ptr
               SDLKeyDown         -> peekKey KeyDown ptr
               SDLKeyUp           -> peekKey KeyUp ptr
               SDLMouseMotion     -> peekMouseMotion ptr
               SDLMouseButtonDown -> peekMouse MouseButtonDown ptr
               SDLMouseButtonUp   -> peekMouse MouseButtonUp ptr
               SDLJoyAxisMotion   -> peekJoyAxisMotion ptr
               SDLJoyBallMotion   -> peekJoyBallMotion ptr
               SDLJoyHatMotion    -> peekJoyHatMotion ptr
               SDLJoyButtonDown   -> peekJoyButton JoyButtonDown ptr
               SDLJoyButtonUp     -> peekJoyButton JoyButtonUp ptr
               SDLQuit            -> return Quit
--           SDLSysWMEvent
               SDLVideoResize     -> peekResize ptr
               SDLVideoExpose     -> return VideoExpose
               SDLUserEvent n     -> peekUserEvent ptr n
--           SDLNumEvents           
               e                  -> failWithError $ "Unhandled eventtype: " ++ show e

-- int SDL_EnableKeyRepeat(int delay, int interval);
foreign import ccall unsafe "SDL_EnableKeyRepeat" sdlEnableKeyRepeat :: Int -> Int -> IO Int

-- | Sets keyboard repeat rate. Returns @False@ on error.
enableKeyRepeat :: Int -- ^ Initial delay. @0@ to disable.
                -> Int -- ^ Interval.
                -> IO Bool
enableKeyRepeat delay interval
    = intToBool (-1) (sdlEnableKeyRepeat delay interval)

-- int SDL_EnableUNICODE(int enable);
foreign import ccall unsafe "SDL_EnableUNICODE" sdlEnableUnicode :: Int -> IO Int

-- | Enables or disables unicode translation.
enableUnicode :: Bool -> IO ()
enableUnicode enable = sdlEnableUnicode (fromToggle toggle) >>
                       return ()
    where toggle = case enable of
                     True -> Enable
                     False -> Disable

-- | Returns the current state of unicode translation. See also 'enableUnicode'.
queryUnicodeState :: IO Bool
queryUnicodeState = fmap toBool (sdlEnableUnicode (fromToggle Query))

-- char *SDL_GetKeyName(SDLKey key);
foreign import ccall unsafe "SDL_GetKeyName" sdlGetKeyName :: Word32 -> IO CString
{-# LINE 508 "Graphics/UI/SDL/Events.hsc" #-}

-- | Gets the name of an SDL virtual keysym.
getKeyName :: SDLKey -> String
getKeyName key = unsafePerformIO $
                 sdlGetKeyName (fromEnum key) >>= peekCString

-- SDLMod SDL_GetModState(void);
foreign import ccall unsafe "SDL_GetModState" sdlGetModState :: IO Word32
{-# LINE 516 "Graphics/UI/SDL/Events.hsc" #-}

-- | Gets the state of modifier keys.
getModState :: IO [Modifier]
getModState = fmap fromBitmask sdlGetModState

-- void SDL_SetModState(SDLMod modstate);
foreign import ccall unsafe "SDL_SetModState" sdlSetModState :: Word32 -> IO ()
{-# LINE 523 "Graphics/UI/SDL/Events.hsc" #-}

-- | Sets the internal state of modifier keys.
setModState :: [Modifier] -> IO ()
setModState = sdlSetModState . toBitmask

mousePressed :: Word8 -> MouseButton -> Bool
mousePressed mask b
    = mask .&. (mouseButtonMask b) /= 0
                  

-- Uint8 SDL_GetMouseState(int *x, int *y);
foreign import ccall "SDL_GetMouseState" sdlGetMouseState :: Ptr CInt -> Ptr CInt -> IO Word8
foreign import ccall "SDL_GetRelativeMouseState" sdlGetRelativeMouseState :: Ptr CInt -> Ptr CInt -> IO Word8

-- | Retrieves the current state of the mouse. Returns (X position, Y position, pressed buttons).
getMouseState :: IO (Int, Int, [MouseButton])
getMouseState = mouseStateGetter sdlGetMouseState

-- | Retrieve the current state of the mouse. Like 'getMouseState' except that X and Y are
--   set to the change since last call to getRelativeMouseState.
getRelativeMouseState :: IO (Int, Int, [MouseButton])
getRelativeMouseState = mouseStateGetter sdlGetRelativeMouseState

mouseStateGetter :: (Ptr CInt -> Ptr CInt -> IO Word8) -> IO  (Int, Int, [MouseButton])
mouseStateGetter getter
    = alloca $ \xPtr ->
      alloca $ \yPtr ->
      do ret <- getter xPtr yPtr
         [x,y] <- mapM peek [xPtr,yPtr]
         return (fromIntegral x,fromIntegral y
                ,filter (mousePressed ret) allButtons)



-- int SDL_PollEvent(SDL_Event *event);
foreign import ccall "SDL_PollEvent" sdlPollEvent :: Ptr Event -> IO Int

-- | Polls for currently pending events.
pollEvent :: IO Event
pollEvent 
    = alloca poll
    where poll ptr
              = do ret <- sdlPollEvent ptr
                   case ret of
                     0 -> return NoEvent
                     _ -> do event <- peek ptr
                             case event of
                               NoEvent -> poll ptr
                               _ -> return event

-- void SDL_PumpEvents(void);
-- | Pumps the event loop, gathering events from the input devices.
foreign import ccall unsafe "SDL_PumpEvents" pumpEvents :: IO ()

-- int SDL_PushEvent(SDL_Event *event);
foreign import ccall unsafe "SDL_PushEvent" sdlPushEvent :: Ptr Event -> IO Int

-- | Pushes an event onto the event queue. Returns @False@ on error.
tryPushEvent :: Event -> IO Bool
tryPushEvent event
    = new event >>= (fmap (0==) . sdlPushEvent)

-- | Pushes an event onto the event queue. Throws an exception on error.
pushEvent :: Event -> IO ()
pushEvent = unwrapBool "SDL_PushEvent" . tryPushEvent

-- int SDL_WaitEvent(SDL_Event *event);
foreign import ccall unsafe "SDL_WaitEvent" sdlWaitEvent :: Ptr Event -> IO Int

-- | Waits indefinitely for the next available event.
waitEvent :: IO Event
waitEvent
    = loop
    where loop = do pumpEvents
                    event <- pollEvent
                    case event of
                      NoEvent -> threadDelay 10 >> loop
                      _ -> return event

-- | Waits indefinitely for the next available event. Blocks Haskell threads.
waitEventBlocking :: IO Event
waitEventBlocking
    = alloca wait
    where wait ptr
              = do ret <- sdlWaitEvent ptr
                   case ret of
                     0 -> failWithError "SDL_WaitEvent"
                     _ -> do event <- peek ptr
                             case event of
                               NoEvent -> wait ptr
                               _ -> return event

-- Uint8 SDL_EventState(Uint8 type, int state);
foreign import ccall unsafe "SDL_EventState" sdlEventState :: Word8 -> Int -> IO Word8

-- |Enable or disable events from being processed.
enableEvent :: SDLEvent -> Bool -> IO ()
enableEvent event on
    = sdlEventState (fromSDLEvent event) (fromToggle state) >> return ()
    where state
              | on = Enable
              | otherwise = Disable

-- |Checks current state of a event. See also 'enableEvent'.
queryEventState :: SDLEvent -> IO Bool
queryEventState event
    = fmap (==1) (sdlEventState (fromSDLEvent event) (fromToggle Query))

-- Uint8 SDL_GetAppState(void);
foreign import ccall unsafe "SDL_GetAppState" sdlGetAppState :: IO Word8

-- | Gets the state of the application.
getAppState :: IO [Focus]
getAppState = fmap fromBitmask sdlGetAppState
