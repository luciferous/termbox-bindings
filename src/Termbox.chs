{-# LANGUAGE ForeignFunctionInterface #-}
module Termbox
  ( Event(..)
  , Cell(..)
  , tbChangeCell
  , tbClear
  , tbHeight
  , tbHideCursor
  , tbInit
  , tbInputMode
  , tbOutputMode
  , tbPeekEvent
  , tbPollEvent
  , tbPresent
  , tbPutCell
  , tbSelectInputMode
  , tbSelectOutputMode
  , tbSetClearAttributes
  , tbSetCursor
  , tbShutdown
  , tbWidth
  ) where

import Control.Monad (void)
import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Termbox.Modes
  ( InputMode, OutputMode
  , toInputMode, toOutputMode
  , fromInputMode, fromOutputMode
  )

import Prelude hiding (mod)

#include <termbox.h>

data Event = KeyEvent Word8 Word16 Word32
           | ResizeEvent Int32 Int32
           | MouseEvent Int32 Int32 Word16
           deriving (Show, Eq)

{#pointer *tb_cell as Cell foreign newtype #}

{#pointer *tb_event as RawEvent foreign newtype #}

{#fun unsafe tb_init as tbInit' {} -> `Int' #}

tbInit :: IO (Either String ())
tbInit = fmap go tbInit'
  where
    go ({#const TB_EUNSUPPORTED_TERMINAL #}) = Left "tb_init: unsupported terminal"
    go ({#const TB_EFAILED_TO_OPEN_TTY #})   = Left "tb_init: failed to open TTY"
    go ({#const TB_EPIPE_TRAP_ERROR #})      = Left "tb_init: pipe trap failed"
    go x = if x <= 0 then Right () else Left "tb_init: unknown"

{#fun unsafe tb_shutdown as ^ {} -> `()' #}

{#fun unsafe tb_width as ^ {} -> `Int' #}
{#fun unsafe tb_height as ^ {} -> `Int' #}

{#fun unsafe tb_clear as ^ {} -> `()' #}
{#fun unsafe tb_set_clear_attributes as ^ {`CUShort', `CUShort'} -> `()' #}

{#fun unsafe tb_present as ^ {} -> `()' #}

tbHideCursor :: IO ()
tbHideCursor = tbSetCursor ({#const TB_HIDE_CURSOR #}) ({#const TB_HIDE_CURSOR #})

{#fun unsafe tb_set_cursor as ^ {`Int', `Int'} -> `()' #}

{#fun unsafe tb_put_cell as ^ {`Int', `Int', `Cell'} -> `()' #}

{#fun unsafe tb_change_cell as ^ {`Int', `Int', `CUInt', `CUShort', `CUShort'} -> `()' #}

{#fun unsafe tb_select_input_mode as tbSelectInputMode' {`Int'} -> `Int' #}

{#fun unsafe tb_select_output_mode as tbSelectOutputMode' {`Int'} -> `Int' #}

tbInputMode :: IO InputMode
tbInputMode = fmap toInputMode (tbSelectInputMode' {#const TB_INPUT_CURRENT #})

tbSelectInputMode :: InputMode -> IO ()
tbSelectInputMode = void . tbSelectInputMode' . fromInputMode

tbOutputMode :: IO OutputMode
tbOutputMode = fmap toOutputMode (tbSelectOutputMode' {#const TB_OUTPUT_CURRENT #})

tbSelectOutputMode :: OutputMode -> IO ()
tbSelectOutputMode = void . tbSelectOutputMode' . fromOutputMode

{#fun tb_peek_event as ^ {+, `Int'} -> `RawEvent' #}

{#fun tb_poll_event as tbPollEvent' {+} -> `RawEvent' #}

tbPollEvent :: IO (Either String Event)
tbPollEvent = tbPollEvent' >>= \e -> withRawEvent e peekEvent

peekEvent :: Ptr RawEvent -> IO (Either String Event)
peekEvent p = {#get tb_event.type #} p >>= toEvent
  where
    toEvent {#const TB_EVENT_KEY #} = fmap Right $
      KeyEvent <$> (fromIntegral <$> {#get tb_event.mod #} p)
               <*> (fromIntegral <$> {#get tb_event.key #} p)
               <*> (fromIntegral <$> {#get tb_event.ch #} p)
    toEvent {#const TB_EVENT_RESIZE #} = fmap Right $
      ResizeEvent <$> (fromIntegral <$> {#get tb_event.w #} p)
                  <*> (fromIntegral <$> {#get tb_event.h #} p)
    toEvent {#const TB_EVENT_MOUSE #} = fmap Right $
      MouseEvent <$> (fromIntegral <$> {#get tb_event.x #} p)
                 <*> (fromIntegral <$> {#get tb_event.y #} p)
                 <*> (fromIntegral <$> {#get tb_event.key #} p)
    toEvent _ = return (Left "tbPollEvent: invalid event type")
