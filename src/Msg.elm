module Msg exposing (Msg(..))

import Types exposing (..)
import Window

type Msg
  = WindowResize Window.Size
  | FocusTab Window
  | DragStart Frame Position
  | DragAt Position
  | DragEnd Position
