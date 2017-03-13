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
  div [ style [ "width" => "100%", "height" => "100%" ] ]
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
    Frame w h Horiz c ->
      div [ frameStyle w h "row" [] ]
        ( List.intersperse
            (div [ style [ "display" => "inline-block", "width" => "2px", "background" => "red", "cursor" => "ew-resize" ] ] [])
            (frameChildren (w // 2) h c)
        )
    Frame w h Vert c ->
      div [ frameStyle w h "column" [] ]
        ( List.intersperse
            (div [ style [ "height" => "2px", "background" => "blue", "cursor" => "ns-resize" ] ] [])
            (frameChildren w (h // 2) c)
        )
    Frame w h None c ->
      div [ frameStyle w h "row" [ "border" => "1px solid #aaa" ] ]
        ( frameChildren w h c)

window : Int -> Int -> Window -> Html Msg
window w h f =
  text "w "


-- STYLES

frameStyle w h d e =
  style
    ([ "display" => "inline-block"
    , "width" => (toString w ++ "px")
    , "height" => (toString h ++ "px")
    , "vertical-align" => "top"
    ] ++ e)
