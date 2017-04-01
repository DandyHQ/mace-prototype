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

    ResizeStart f xy ->
      ( { model | resizeDrag = Just (ResizeDrag f xy xy) }, Cmd.none )

    ResizeAt xy ->
      ( { model | resizeDrag = Maybe.map (\{frame, start} -> ResizeDrag frame start xy) model.resizeDrag }, Cmd.none )

    ResizeEnd _ ->
      ( { model | resizeDrag = Nothing, frames = Frame.resize model.resizeDrag model.frames }, Cmd.none )
