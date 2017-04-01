module Msg exposing (Msg(..))

import Types exposing (..)
import Window

type Msg
  = WindowResize Window.Size
  | FocusTab Window
  | ResizeStart Frame Position
  | ResizeAt Position
  | ResizeEnd Position
