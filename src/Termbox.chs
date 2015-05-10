{-# LANGUAGE ForeignFunctionInterface #-}
module Termbox
  ( C.Cell
  , C.changeCell
  , C.clear
  , C.height
  , C.present
  , C.putCell
  , C.setClearAttr
  , C.setCursor
  , C.shutdown
  , C.width
  , Event(..)
  , InputMode(..)
  , OutputMode(..)
  , getInputMode
  , getOutputMode
  , hideCursor
  , init
  , inputMode
  , peekEvent
  , pollEvent
  , setInputMode
  , setOutputMode
  ) where

import Control.Arrow (left)
import Control.Monad (void)
import Data.Bits
import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import Prelude hiding (init, mod)

import qualified Termbox.C     as C
import qualified Termbox.Enums as E

#include <termbox.h>

-- | A termbox event.
data Event
  = KeyEvent Word8 Word16 Word32  -- ^ A key event: mod, key, ch
  | ResizeEvent Int32 Int32       -- ^ A resize event: w, h
  | MouseEvent Int32 Int32 Word16 -- ^ A mouse event: w, h, key
  deriving (Show, Eq)

-- | Termbox input modes.
data InputMode = InputMode
  { isEsc :: Bool   -- ^ ESC means "E.TB_KEY_ESC"
  , isAlt :: Bool   -- ^ ESC enables "E.TB_MOD_ALT" for next key event
  , isMouse :: Bool -- ^ Enable mouse events
  } deriving (Show, Eq)

inputMode :: InputMode
inputMode = InputMode False False False

-- | Termbox output modes.
data OutputMode
  = Normal      -- ^ 8 colors.
  | M256        -- ^ 256 colors.
  | M216        -- ^ 216 colors.
  | Grayscale   -- ^ 24 shades of gray.
  | Unknown Int -- ^ A mystery.
  deriving (Show, Eq)

-- | Initializes termbox.
init :: IO (Either String ())
init = check <$> C.tb_init
  where
    check e
      | e == {#const TB_EUNSUPPORTED_TERMINAL #} = Left "tb_init: unsupported terminal"
      | e == {#const TB_EFAILED_TO_OPEN_TTY #}   = Left "tb_init: failed to open TTY"
      | e == {#const TB_EPIPE_TRAP_ERROR #}      = Left "tb_init: pipe trap failed"
      | e < 0                                    = Left ("tb_init: errno " ++ (show e))
      | otherwise                                = Right ()

hideCursor :: IO ()
hideCursor = C.setCursor x x where x = {#const TB_HIDE_CURSOR #}

getInputMode :: IO InputMode
getInputMode = conv <$> (C.tb_select_input_mode {#const TB_INPUT_CURRENT #})
  where
    conv bits = InputMode { isEsc   = 0 /= bits .&. {#const TB_INPUT_ESC #}
                          , isAlt   = 0 /= bits .&. {#const TB_INPUT_ALT #}
                          , isMouse = 0 /= bits .&. {#const TB_INPUT_MOUSE #}
                          }

setInputMode :: InputMode -> IO ()
setInputMode = void . C.tb_select_input_mode . conv
  where conv mode = let e = if isEsc mode   then {#const TB_INPUT_ESC #}   else 0
                        a = if isAlt mode   then {#const TB_INPUT_ALT #}   else 0
                        m = if isMouse mode then {#const TB_INPUT_MOUSE #} else 0
                     in e .|. a .|. m

getOutputMode :: IO OutputMode
getOutputMode = (conv . toEnum) <$> C.tb_select_output_mode {#const TB_OUTPUT_CURRENT #}
  where
    conv i
      | i == E.TB_OUTPUT_NORMAL    = Normal
      | i == E.TB_OUTPUT_256       = M256
      | i == E.TB_OUTPUT_216       = M216
      | i == E.TB_OUTPUT_GRAYSCALE = Grayscale
      | otherwise                  = Unknown (fromEnum i)

setOutputMode :: OutputMode -> IO ()
setOutputMode = void . C.tb_select_output_mode . conv
  where
    conv Normal      = {#const TB_OUTPUT_NORMAL #}
    conv M256        = {#const TB_OUTPUT_256 #}
    conv M216        = {#const TB_OUTPUT_216 #}
    conv Grayscale   = {#const TB_OUTPUT_GRAYSCALE #}
    conv (Unknown i) = fromIntegral i

-- | Wait for an event with a timeout.
peekEvent :: Int -> IO (Maybe Event)
peekEvent t = fmap (either (const Nothing) Just)
                   (C.tb_peek_event t >>= (flip C.withEvent) handleEvent)

-- | Wait for an event.
pollEvent :: IO (Either String Event)
pollEvent = fmap (left showError)
                 (C.tb_poll_event >>= (flip C.withEvent) handleEvent)
  where showError i = "tb_poll_event: invalid event type" ++ (show i)

handleEvent :: Ptr C.Event -> IO (Either Int Event)
handleEvent p = {#get tb_event.type #} p >>= conv
  where
    conv :: CUChar -> IO (Either Int Event)
    conv {#const TB_EVENT_KEY #} = fmap Right $
      KeyEvent <$> (fromIntegral <$> {#get tb_event.mod #} p)
               <*> (fromIntegral <$> {#get tb_event.key #} p)
               <*> (fromIntegral <$> {#get tb_event.ch #} p)
    conv {#const TB_EVENT_RESIZE #} = fmap Right $
      ResizeEvent <$> (fromIntegral <$> {#get tb_event.w #} p)
                  <*> (fromIntegral <$> {#get tb_event.h #} p)
    conv {#const TB_EVENT_MOUSE #} = fmap Right $
      MouseEvent <$> (fromIntegral <$> {#get tb_event.x #} p)
                 <*> (fromIntegral <$> {#get tb_event.y #} p)
                 <*> (fromIntegral <$> {#get tb_event.key #} p)
    conv i = return (Left (fromIntegral i))
