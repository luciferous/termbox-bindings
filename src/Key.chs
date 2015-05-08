{-# LANGUAGE ForeignFunctionInterface #-}
module Termbox.Key (Key, Mod) where

import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

#include <termbox.h>

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

