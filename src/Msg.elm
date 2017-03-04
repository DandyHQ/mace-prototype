module Msg exposing (Msg(..))

import Types

type Msg
  = NewWindow Int Types.Tile
  | FocusTab Types.Frame
