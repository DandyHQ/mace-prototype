module Model exposing (Model, Frame(..), Tile(..), init)

import Msg exposing (Msg)

type Frame = Frame Tile (List Frame) | Window Int

type Tile = Horiz | Vert | Tabbed

type alias Model =
  { windowIndex : Int
  , frames : Frame
  }

init : ( Model, Cmd Msg )
init =
  ( Model 3 (Frame Horiz [ Window 1, Frame Horiz [ Window 2, Window 3 ] ]), Cmd.none )
