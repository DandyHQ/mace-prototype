module View exposing (view)

import Array
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Msg exposing (Msg)
import Types exposing (..)

(=>) =
  (,)

view : Model -> Html Msg
view model =
  div [ style [ "width" => "100%", "height" => "100%" ] ]
    [ frame 100 100 None model.frames ]

frameChildren : Int -> Int -> Tile -> FrameChildren -> List (Html Msg)
frameChildren w h t f =
  case f of
    FrameFrame l ->
      List.map (frame w h t) l
    WindowFrame l ->
      List.map (window w h) l

frame : Int -> Int -> Tile -> Frame -> Html Msg
frame w h t f =
  case f of
    Frame w h o Horiz c ->
      div [ frameStyle w h t o "row" [] ]
        ( List.intersperse
            (div [ style [ "display" => "inline-block", "width" => "2px", "background" => "red", "cursor" => "ew-resize" ] ] [])
            (frameChildren (w // 2) h Horiz c)
        )
    Frame w h o Vert c ->
      div [ frameStyle w h t o "column" [] ]
        ( List.intersperse
            (div [ style [ "height" => "2px", "background" => "blue", "cursor" => "ns-resize" ] ] [])
            (frameChildren w (h // 2) Vert c)
        )
    Frame w h o None c ->
      div [ frameStyle w h t o "row" [ "border" => "1px solid #aaa" ] ]
        ( frameChildren w h None c)

window : Int -> Int -> Window -> Html Msg
window w h f =
  text "w "


-- STYLES

frameStyle w h t o d e =
  style
    ([ "position" => "absolute"
    , "width" => (toString w ++ "px")
    , "height" => (toString h ++ "px")
    , "vertical-align" => "top"
    ]
    ++ (if t == Horiz then ["left" => (toString o ++ "px"), "top" => "0"] else if t == Vert then ["top" => (toString o ++ "px"), "left" => "0"] else [])
    ++ e)
