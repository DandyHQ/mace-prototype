module Update exposing (update)

import Frame
import Model exposing (Model)
import Msg exposing (Msg(..))
import Types exposing (..)
import Window

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    WindowResize newSize ->
      ( { model | window = newSize, frames = Frame.resizeAll model.window newSize model.frames }, Cmd.none )

    FocusTab window ->
      ( { model | frames = Frame.focus window model.frames }, Cmd.none )

    DragStart f xy ->
      ( { model | drag = Just (Drag f xy xy) }, Cmd.none )

    DragAt xy ->
      ( { model | drag = Maybe.map (\{frame, start} -> Drag frame start xy) model.drag }, Cmd.none )

    DragEnd _ ->
      ( { model | drag = Nothing, frames = Frame.resize model.drag model.frames }, Cmd.none )
