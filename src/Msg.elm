module Msg exposing (Msg(..))

import Types exposing (..)
import Window

type Msg
  = WindowResize Window.Size
  | ResizeStart Frame Position
  | MoveStart Tab Position
  | DragAt Position
  | DragEnd Position
