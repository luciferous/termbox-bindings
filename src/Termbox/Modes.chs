module Termbox.Modes where

import Data.Bits

#include <termbox.h>

data InputMode = InputMode
  { isEsc :: Bool
  , isAlt :: Bool
  , isMouse :: Bool
  } deriving (Show, Eq)

toInputMode :: Int -> InputMode
toInputMode bits = InputMode
  { isEsc   = 0 /= bits .&. {#const TB_INPUT_ESC #}
  , isAlt   = 0 /= bits .&. {#const TB_INPUT_ALT #}
  , isMouse = 0 /= bits .&. {#const TB_INPUT_MOUSE #}
  }

fromInputMode :: InputMode -> Int
fromInputMode mode =
  let e = if isEsc mode   then {#const TB_INPUT_ESC #} else 0
      a = if isAlt mode   then {#const TB_INPUT_ALT #} else 0
      m = if isMouse mode then {#const TB_INPUT_MOUSE #} else 0
  in e .|. a .|. m

inputMode :: InputMode
inputMode = InputMode False False False

data OutputMode = OutputMode
  { isNormal :: Bool
  , is256 :: Bool
  , is216 :: Bool
  , isGrayscale :: Bool
  } deriving (Show, Eq)

toOutputMode :: Int -> OutputMode
toOutputMode bits = OutputMode
  { isNormal    = 0 /= bits .&. {#const TB_OUTPUT_NORMAL #}
  , is256       = 0 /= bits .&. {#const TB_OUTPUT_256 #}
  , is216       = 0 /= bits .&. {#const TB_OUTPUT_216 #}
  , isGrayscale = 0 /= bits .&. {#const TB_OUTPUT_GRAYSCALE #}
  }

fromOutputMode :: OutputMode -> Int
fromOutputMode mode =
  let n = if isNormal mode    then {#const TB_OUTPUT_NORMAL #} else 0
      h = if is256 mode       then {#const TB_OUTPUT_256 #} else 0
      l = if is216 mode       then {#const TB_OUTPUT_216 #} else 0
      g = if isGrayscale mode then {#const TB_OUTPUT_GRAYSCALE #} else 0
  in n .|. h .|. l .|. g

outputMode :: OutputMode
outputMode = OutputMode False False False False
