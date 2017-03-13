module Update exposing (update)

import Model exposing (Model)
import Msg exposing (Msg(..))
import Types exposing (Frame(..), Tile(..))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    WindowResize s ->
      ( { model | window = s }, Cmd.none )
