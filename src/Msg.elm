module Msg exposing (Msg(..))

import Types
import Window

type Msg
  = WindowResize Window.Size
