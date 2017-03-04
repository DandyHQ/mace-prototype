module Model exposing (Model, init)

import Msg exposing (Msg)

type alias Model =
  String

init : ( Model, Cmd Msg )
init =
  ( "", Cmd.none )
