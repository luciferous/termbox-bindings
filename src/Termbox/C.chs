{-# LANGUAGE ForeignFunctionInterface #-}
module Termbox.C
  ( changeCell
  , clear
  , height
  , present
  , putCell
  , setClearAttr
  , setCursor
  , shutdown
  , width

  -- | Low level
  , Event, withEvent
  , Cell, withCell
  , tb_init
  , tb_peek_event
  , tb_poll_event
  , tb_select_input_mode
  , tb_select_output_mode
  ) where

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr

#include <termbox.h>

{#pointer *tb_cell as Cell foreign newtype #}
{#pointer *tb_event as Event foreign newtype #}
{#fun unsafe tb_init {} -> `Int' #}
{#fun unsafe tb_peek_event {+, `Int'} -> `Event' #}
{#fun unsafe tb_poll_event {+} -> `Event' #}
{#fun unsafe tb_select_input_mode {`Int'} -> `Int' #}
{#fun unsafe tb_select_output_mode {`Int'} -> `Int' #}

{#fun unsafe tb_change_cell as changeCell {`Int', `Int', `CUInt', `CUShort', `CUShort'} -> `()' #}
{#fun unsafe tb_clear as clear {} -> `()' #}
{#fun unsafe tb_height as height {} -> `Int' #}
{#fun unsafe tb_present as present {} -> `()' #}
{#fun unsafe tb_put_cell as putCell { `Int', `Int', `Cell' } -> `()' #}
{#fun unsafe tb_set_clear_attributes as setClearAttr {`CUShort', `CUShort'} -> `()' #}
{#fun unsafe tb_set_cursor as setCursor {`Int', `Int'} -> `()' #}
{#fun unsafe tb_shutdown as shutdown {} -> `()' #}
{#fun unsafe tb_width as width {} -> `Int' #}

