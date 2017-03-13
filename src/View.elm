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
    Frame i s Horiz c ->
      let
        children = frameChildren (w // 2) h c
        borders = List.map (\v -> div [ onClick (Msg.Resize i v), style [ "width" => "2px", "background" => "red", "cursor" => "ew-resize" ] ] []) (List.range 0 (List.length children))
        elems = List.drop 1 (List.concat (List.map2 (\a b -> a :: b :: []) borders children))
      in
      div [ frameStyle s "row" [] ]
        elems
    Frame i s Vert c ->
      let
        children = frameChildren w (h // 2) c
        borders = List.map (\v -> div [ onClick (Msg.Resize i v), style [ "height" => "2px", "background" => "blue", "cursor" => "ns-resize" ] ] []) (List.range 0 (List.length children))
        elems = List.drop 1 (List.concat (List.map2 (\a b -> a :: b :: []) borders children))
      in
      div [ frameStyle s "column" [] ]
        elems
    Frame i s None c ->
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
