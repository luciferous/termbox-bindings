{-# LANGUAGE ForeignFunctionInterface #-}
module Termbox where

import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

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

data Event = Key Word8 Word16 Word32
           | Resize Word32 Word32
           | Mouse Word32 Word32 Word16
           deriving (Show, Eq)

instance Storable Event where
  sizeOf _ = {#sizeof tb_event #}
  alignment _ = {#alignof tb_event #}
  peek p = {#get tb_event.type #} p >>= peek' p
    where
      peek' p 1 = Key <$> (fromIntegral <$> {#get tb_event.mod #} p)
                      <*> (fromIntegral <$> {#get tb_event.key #} p)
                      <*> (fromIntegral <$> {#get tb_event.ch #} p)
      peek' p 2 = Resize <$> (fromIntegral <$> {#get tb_event.w #} p)
                         <*> (fromIntegral <$> {#get tb_event.h #} p)
      peek' p 3 = Mouse <$> (fromIntegral <$> {#get tb_event.x #} p)
                        <*> (fromIntegral <$> {#get tb_event.y #} p)
                        <*> (fromIntegral <$> {#get tb_event.key #} p)
      peek' _ _ = error "Invalid event type"
  poke p (Key mod key ch) =
       ({#set tb_event.mod #} p $ fromIntegral mod)
    *> ({#set tb_event.key #} p $ fromIntegral key)
    *> ({#set tb_event.ch #} p $ fromIntegral ch)
  poke p (Resize w h) =
       ({#set tb_event.w #} p $ fromIntegral w)
    *> ({#set tb_event.h #} p $ fromIntegral h)
  poke p (Mouse x y key) =
       ({#set tb_event.x #} p $ fromIntegral x)
    *> ({#set tb_event.y #} p $ fromIntegral y)
    *> ({#set tb_event.key #} p $ fromIntegral key)

{#pointer *tb_event as EventPtr -> Event #}

{#enum define Key {
  TB_KEY_F1               as F1,
  TB_KEY_F2               as F2,
  TB_KEY_F3               as F3,
  TB_KEY_F4               as F4,
  TB_KEY_F5               as F5,
  TB_KEY_F6               as F6,
  TB_KEY_F7               as F7,
  TB_KEY_F8               as F8,
  TB_KEY_F9               as F9,
  TB_KEY_F10              as F10,
  TB_KEY_F11              as F11,
  TB_KEY_F12              as F12,
  TB_KEY_INSERT           as Insert,
  TB_KEY_DELETE           as Delete,
  TB_KEY_HOME             as Home,
  TB_KEY_END              as End,
  TB_KEY_PGUP             as Pgup,
  TB_KEY_PGDN             as Pgdn,
  TB_KEY_ARROW_UP         as ArrowUp,
  TB_KEY_ARROW_DOWN       as ArrowDown,
  TB_KEY_ARROW_LEFT       as ArrowLeft,
  TB_KEY_ARROW_RIGHT      as ArrowRight,
  TB_KEY_MOUSE_LEFT       as MouseLeft,
  TB_KEY_MOUSE_RIGHT      as MouseRight,
  TB_KEY_MOUSE_MIDDLE     as MouseMiddle,
  TB_KEY_MOUSE_RELEASE    as MouseRelease,
  TB_KEY_MOUSE_WHEEL_UP   as MouseWheelUp,
  TB_KEY_MOUSE_WHEEL_DOWN as MouseWheelDown,
  TB_KEY_CTRL_TILDE       as CtrlTilde,
  TB_KEY_CTRL_2           as Ctrl_2,
  TB_KEY_CTRL_A           as CtrlA,
  TB_KEY_CTRL_B           as CtrlB,
  TB_KEY_CTRL_C           as CtrlC,
  TB_KEY_CTRL_D           as CtrlD,
  TB_KEY_CTRL_E           as CtrlE,
  TB_KEY_CTRL_F           as CtrlF,
  TB_KEY_CTRL_G           as CtrlG,
  TB_KEY_BACKSPACE        as Backspace,
  TB_KEY_CTRL_H           as CtrlH,
  TB_KEY_TAB              as Tab,
  TB_KEY_CTRL_I           as CtrlI,
  TB_KEY_CTRL_J           as CtrlJ,
  TB_KEY_CTRL_K           as CtrlK,
  TB_KEY_CTRL_L           as CtrlL,
  TB_KEY_ENTER            as Enter,
  TB_KEY_CTRL_M           as CtrlM,
  TB_KEY_CTRL_N           as CtrlN,
  TB_KEY_CTRL_O           as CtrlO,
  TB_KEY_CTRL_P           as CtrlP,
  TB_KEY_CTRL_Q           as CtrlQ,
  TB_KEY_CTRL_R           as CtrlR,
  TB_KEY_CTRL_S           as CtrlS,
  TB_KEY_CTRL_T           as CtrlT,
  TB_KEY_CTRL_U           as CtrlU,
  TB_KEY_CTRL_V           as CtrlV,
  TB_KEY_CTRL_W           as CtrlW,
  TB_KEY_CTRL_X           as CtrlX,
  TB_KEY_CTRL_Y           as CtrlY,
  TB_KEY_CTRL_Z           as CtrlZ,
  TB_KEY_ESC              as Esc,
  TB_KEY_CTRL_LSQ_BRACKET as CtrlLsqBracket,
  TB_KEY_CTRL_3           as Ctrl_3,
  TB_KEY_CTRL_4           as Ctrl_4,
  TB_KEY_CTRL_BACKSLASH   as CtrlBackslash,
  TB_KEY_CTRL_5           as Ctrl_5,
  TB_KEY_CTRL_RSQ_BRACKET as CtrlRsqBracket,
  TB_KEY_CTRL_6           as Ctrl_6,
  TB_KEY_CTRL_7           as Ctrl_7,
  TB_KEY_CTRL_SLASH       as CtrlSlash,
  TB_KEY_CTRL_UNDERSCORE  as CtrlUnderscore,
  TB_KEY_SPACE            as Space,
  TB_KEY_BACKSPACE2       as Backspace2,
  TB_KEY_CTRL_8           as Ctrl_8
} #}

{#enum define Mod {
  TB_MOD_ALT as Alt
} #}

{#enum define Color {
  TB_DEFAULT as Default,
  TB_BLACK   as Black,
  TB_RED     as Red,
  TB_GREEN   as Green,
  TB_YELLOW  as Yellow,
  TB_BLUE    as Blue,
  TB_MAGENTA as Magenta,
  TB_CYAN    as Cyan,
  TB_WHITE   as White
} #}

{#enum define Attr {
  TB_BOLD      as Bold,
  TB_UNDERLINE as Underline,
  TB_REVERSE   as Reverse
} #}

{#enum define Errors {
  TB_EUNSUPPORTED_TERMINAL as UnsupportedTerminal,
  TB_EFAILED_TO_OPEN_TTY   as FailedToOpenTty,
  TB_EPIPE_TRAP_ERROR      as PipeTrapError
} #}

{#fun unsafe tb_init as ^ {} -> `Int' #}
{#fun unsafe tb_shutdown as ^ {} -> `()' #}

{#fun unsafe tb_width as ^ {} -> `Int' #}
{#fun unsafe tb_height as ^ {} -> `Int' #}

{#fun unsafe tb_clear as ^ {} -> `()' #}
{#fun unsafe tb_set_clear_attributes as ^ {`CUShort', `CUShort'} -> `()' #}

{#fun unsafe tb_present as ^ {} -> `()' #}

{#enum define Special {
  TB_HIDE_CURSOR as HideCursor
} #}

{#fun unsafe tb_set_cursor as ^ {`Int', `Int'} -> `()' #}

{#fun unsafe tb_put_cell as ^ {`Int', `Int', with* `Cell'} -> `()' #}

{#fun unsafe tb_change_cell as ^ {`Int', `Int', `CUInt', `CUShort', `CUShort'} -> `()' #}

{#enum define InputMode {
  TB_INPUT_CURRENT as InputCurrent,
  TB_INPUT_ESC     as InputEsc,
  TB_INPUT_ALT     as InputAlt,
  TB_INPUT_MOUSE   as InputMouse
} #}

{#fun unsafe tb_select_input_mode as ^ {`Int'} -> `Int' #}

{#enum define OutputMode {
  TB_OUTPUT_CURRENT   as OutputCurrent,
  TB_OUTPUT_NORMAL    as OutputNormal,
  TB_OUTPUT_256       as Output256,
  TB_OUTPUT_216       as Output216,
  TB_OUTPUT_GRAYSCALE as OutputGrayscale
} #}

{#fun unsafe tb_select_output_mode as ^ {`Int'} -> `Int' #}

{#fun unsafe tb_peek_event as ^ {alloca- `Event' peek*, `Int'} -> `Int' #}

{#fun unsafe tb_poll_event as ^ {alloca- `Event' peek*} -> `Int' #}
