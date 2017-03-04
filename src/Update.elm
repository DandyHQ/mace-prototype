module Update exposing (update)

import Model exposing (Model)
import Msg exposing (Msg(..))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewWindow id ->
      ( { model | windowIndex = model.windowIndex + 1
        , frames = newWindow model.windowIndex id model.frames
        }
      , Cmd.none)

newWindow : Int -> Int -> Model.Frame -> Model.Frame
newWindow index target model =
  case model of
    Model.Frame children ->
      Model.Frame (List.map (newWindow index target) children)
    Model.Window id ->
      if id == target then
        Model.Frame [ Model.Window id, Model.Window (index + 1) ]
      else
        model
