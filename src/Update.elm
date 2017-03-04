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

    FocusTab frame ->
      ( { model | frames = focusTab frame model.frames }, Cmd.none)

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

focusTab : Frame -> Frame -> Frame
focusTab target model =
  case model of
    Frame Horiz children ->
      Frame Horiz (List.map (focusTab target) children)
    Frame Vert children ->
      Frame Vert (List.map (focusTab target) children)
    Frame (Tabbed focused) children ->
      let haveTarget =
        List.filter ((==) target) children
      in
      if List.length haveTarget /= 0 then
        Frame (Tabbed (List.sum (List.indexedMap (\k v -> if v == target then k else 0) children))) children
      else
        Frame (Tabbed focused) (List.map (focusTab target) children)


    Window id ->
      Window id
