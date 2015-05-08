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
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Termbox.Modes
  ( InputMode, OutputMode
  , toInputMode, toOutputMode
  , fromInputMode, fromOutputMode
  )

import Prelude hiding (mod)

#include <termbox.h>

data Cell = Cell Word32 Word16 Word16

instance Storable Cell where
  sizeOf _ = {#sizeof tb_cell #}
  alignment _ = {#alignof tb_cell #}
  peek p = Cell <$> (fromIntegral <$> {#get tb_cell.ch #} p)
                <*> (fromIntegral <$> {#get tb_cell.fg #} p)
                <*> (fromIntegral <$> {#get tb_cell.bg #} p)
  poke p (Cell ch fg bg) =
       ({#set tb_cell.ch #} p $ fromIntegral ch)
    *> ({#set tb_cell.fg #} p $ fromIntegral fg)
    *> ({#set tb_cell.bg #} p $ fromIntegral bg)

{#pointer *tb_cell as CellPtr -> Cell #}

data Event = KeyEvent Word8 Word16 Word32
           | ResizeEvent Word32 Word32
           | MouseEvent Word32 Word32 Word16
           deriving (Show, Eq)

instance Storable Event where
  sizeOf _ = {#sizeof tb_event #}
  alignment _ = {#alignof tb_event #}
  peek p = {#get tb_event.type #} p >>= peek'
    where
      peek' {#const TB_EVENT_KEY #} =
        KeyEvent <$> (fromIntegral <$> {#get tb_event.mod #} p)
                 <*> (fromIntegral <$> {#get tb_event.key #} p)
                 <*> (fromIntegral <$> {#get tb_event.ch #} p)
      peek' {#const TB_EVENT_RESIZE #} =
        ResizeEvent <$> (fromIntegral <$> {#get tb_event.w #} p)
                    <*> (fromIntegral <$> {#get tb_event.h #} p)
      peek' {#const TB_EVENT_MOUSE #} =
        MouseEvent <$> (fromIntegral <$> {#get tb_event.x #} p)
                   <*> (fromIntegral <$> {#get tb_event.y #} p)
                   <*> (fromIntegral <$> {#get tb_event.key #} p)
      peek' _ = error "Invalid event type"
  poke p (KeyEvent mod key ch) =
       ({#set tb_event.mod #} p $ fromIntegral mod)
    *> ({#set tb_event.key #} p $ fromIntegral key)
    *> ({#set tb_event.ch #} p $ fromIntegral ch)
  poke p (ResizeEvent w h) =
       ({#set tb_event.w #} p $ fromIntegral w)
    *> ({#set tb_event.h #} p $ fromIntegral h)
  poke p (MouseEvent x y key) =
       ({#set tb_event.x #} p $ fromIntegral x)
    *> ({#set tb_event.y #} p $ fromIntegral y)
    *> ({#set tb_event.key #} p $ fromIntegral key)

{#pointer *tb_event as EventPtr -> Event #}

{#fun unsafe tb_init as tbInit' {} -> `Int' #}

tbInit :: IO (Either String ())
tbInit = fmap go tbInit'
  where
    go ({#const TB_EUNSUPPORTED_TERMINAL #}) = Left "tb_init: unsupported terminal"
    go ({#const TB_EFAILED_TO_OPEN_TTY #})   = Left "tb_init: failed to open TTY"
    go ({#const TB_EPIPE_TRAP_ERROR #})      = Left "tb_init: pipe trap failed"
    go x = if x < 0 then Right () else Left "tb_init: unknown"

{#fun unsafe tb_shutdown as ^ {} -> `()' #}

{#fun unsafe tb_width as ^ {} -> `Int' #}
{#fun unsafe tb_height as ^ {} -> `Int' #}

{#fun unsafe tb_clear as ^ {} -> `()' #}
{#fun unsafe tb_set_clear_attributes as ^ {`CUShort', `CUShort'} -> `()' #}

{#fun unsafe tb_present as ^ {} -> `()' #}

tbHideCursor :: IO ()
tbHideCursor = tbSetCursor ({#const TB_HIDE_CURSOR #}) ({#const TB_HIDE_CURSOR #})

{#fun unsafe tb_set_cursor as ^ {`Int', `Int'} -> `()' #}

{#fun unsafe tb_put_cell as ^ {`Int', `Int', with* `Cell'} -> `()' #}

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

{#fun unsafe tb_peek_event as ^ {alloca- `Event' peek*, `Int'} -> `Int' #}

{#fun unsafe tb_poll_event as tbPollEvent' {alloca- `Event' peek*} -> `Int' #}

tbPollEvent :: IO (Either String Event)
tbPollEvent = fmap go tbPollEvent'
  where
    go (-1, _) = Left "error: tb_poll_event returned -1"
    go (_,  e) = Right e
