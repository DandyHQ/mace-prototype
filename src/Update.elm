module Update exposing (update)

import Model exposing (Model)
import Msg exposing (Msg(..))
import Types exposing (Frame(..), Tile(..))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewWindow id t ->
      ( { model | nextIndex = model.nextIndex + 1
        , frames = newWindow model.nextIndex id t model.frames
        }
      , Cmd.none)

newWindow : Int -> Int -> Tile -> Frame -> Frame
newWindow index target t model =
  case model of
    Frame tile children ->
      Frame tile (List.map (newWindow index target t) children)
    Window id ->
      if id == target then
        Frame t [ Window id, Window index ]
      else
        model
