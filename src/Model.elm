module Model exposing (Model, init)

import Msg exposing (Msg)
import Types exposing (Frame(..), Tile(..))

type alias Model =
  { nextIndex : Int
  , frames : Frame
  }

init : ( Model, Cmd Msg )
init =
  ( Model 2 (Frame Horiz [ Window 1 ]), Cmd.none )
