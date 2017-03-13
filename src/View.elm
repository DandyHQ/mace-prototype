module View exposing (view)

import Array
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Msg exposing (Msg)
import Types exposing (Window(..), Frame(..), FrameChildren(..), Tile(..))

(=>) =
  (,)

view : Model -> Html Msg
view model =
  frame 500 500 model.frames

frameChildren : Int -> Int -> FrameChildren -> List (Html Msg)
frameChildren w h f =
  case f of
    FrameFrame l ->
      List.map (frame w h) l
    WindowFrame l ->
      List.map (window w h) l

frame : Int -> Int -> Frame -> Html Msg
frame w h f =
  case f of
    Frame Horiz c ->
      div [ style [ "width" => (toString w ++ "px"), "height" => (toString h ++ "px"), "background" => "red" ] ]
        ( frameChildren (w // 3) h c)
    Frame Vert c ->
      div [ style [ "width" => (toString w ++ "px"), "height" => (toString h ++ "px"), "background" => "green" ] ]
        ( frameChildren w (h // 3) c)
    Frame None c ->
      div [ style [ "width" => (toString w ++ "px"), "height" => (toString h ++ "px"), "background" => "blue" ] ] [ text "none" ]

window : Int -> Int -> Window -> Html Msg
window w h f =
  text "window"
