module Msg exposing (Msg(..))

import Types

type Msg
  = Resize Int Int -- Resize ParentID Offset
