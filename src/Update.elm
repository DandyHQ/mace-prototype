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
      ( { model | frames = Frame.resizeAll newSize model.frames }, Cmd.none )

    ResizeStart frame xy ->
      ( { model | resizeDrag = Just (ResizeDrag frame xy xy) }, Cmd.none )

    -- having an onMouseDown event snuffs onClick
    -- so both are handled here
    MoveStart tab tabPos xy ->
      ( { model
          | moveDrag = Just (MoveDrag tab False (Position (tabPos.x - xy.x) (tabPos.y - xy.y)) xy xy)
          , frames = Frame.focus tab model.frames
        }, Cmd.none )

    DragAt xy ->
      ( { model
          | resizeDrag = Maybe.map (\{frame, start} -> ResizeDrag frame start xy) model.resizeDrag
          , moveDrag = Maybe.map (\{tab, moved, offset, start} ->
            MoveDrag tab
              (if moved || (abs (xy.x - start.x)) > 2 || (abs (xy.y - start.y)) > 2 then
                True
              else
                False)
              offset start xy
          ) model.moveDrag
        }, Cmd.none )

    DragEnd xy ->
      -- let
      --   positioned = Frame.layoutWindows model.window model.frames
      --   shadow = Frame.tabShadow model.moveDrag positioned
      --   split = Frame.applySplit model.moveDrag shadow model.frames
      -- in
      let
        newResizeDrag = Maybe.map (\{frame, start} -> ResizeDrag frame start xy) model.resizeDrag
        resized = Frame.resize newResizeDrag model.frames
        rearranged = Frame.rearrange model.moveDrag resized
      in
      ( { model
          | resizeDrag = Nothing
          , moveDrag = Nothing
          , frames = rearranged
        }, Cmd.none )
