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
  div [ style [ "width" => "100%", "height" => "100%", "display" => "flex", "direction" => "column" ] ]
    [ frame 100 100 model.frames ]

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
    Frame s Horiz c ->
      div [ frameStyle s "row" [] ]
        ( frameChildren (w // 2) h c)
    Frame s Vert c ->
      div [ frameStyle s "column" [] ]
        ( frameChildren w (h // 2) c)
    Frame s None c ->
      div [ frameStyle s "row" [ "border" => "1px solid #aaa" ] ]
        ( frameChildren w h c)

window : Int -> Int -> Window -> Html Msg
window w h f =
  text "w "


-- STYLES

frameStyle s d e =
  style
    ([ "display" => "flex"
    , "flex-direction" => d
    , "flex-grow" => (toString s) -- as child
    , "vertical-align" => "top"
    ] ++ e)
