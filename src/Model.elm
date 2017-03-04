module Model exposing (Model, Frame(..), init)

import Msg exposing (Msg)

type Frame = Frame (List Frame) | Window Int

type alias Model =
  { windowIndex : Int
  , frames : Frame
  }

init : ( Model, Cmd Msg )
init =
  ( Model 3 (Frame [ Window 1, Frame [ Window 2, Window 3 ] ]), Cmd.none )
