module View exposing (view)

import Array
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, on)
import Model exposing (Model)
import Msg exposing (Msg)
import Types exposing (..)

(=>) =
  (,)

view : Model -> Html Msg
view model =
  div [ style [ "width" => "100%", "height" => "100%", "background-color" => "#c6c6c6" ] ]
    [ frame (Position 0 0) model.window None model.frames ]

frameChildren : Size -> Size -> Tile -> List Frame -> List (Html Msg)
frameChildren size rem tile l =
  case l of
    [] -> []
    hd :: [] ->
      frame (Position (size.width - rem.width) (size.height - rem.height)) rem tile hd :: []
    hd :: tl ->
      frame (Position (size.width - rem.width) (size.height - rem.height)) rem tile hd
        :: case tile of
          Horiz ->
            frameChildren size (Size (size.width - (getSize size tile hd).width - 1) size.height) tile tl
          Vert ->
            frameChildren size (Size size.width (size.height - (getSize size tile hd).height - 1)) tile tl
          -- any remaining cases are the result of an invalid tree
          _ ->
            frameChildren size rem tile tl

frame : Position -> Size -> Tile -> Frame -> Html Msg
frame pos size tile f =
  case f of
    Frame s t c ->
      div [ frameStyle pos (getSize size tile f) t ]
        (case c of
          FrameFrame l ->
            frameChildren (getSize size tile f) (getSize size tile f) t l
          WindowFrame l ->
            [div [] [ text "Hello, World" ]]
        )

getSize : Size -> Tile -> Frame -> Size
getSize size tile f =
  case f of
    Frame s _ _ ->
      case tile of
        None ->
          size
        Horiz ->
          Size s size.height
        Vert ->
          Size size.width s

frameStyle : Position -> Size -> Tile -> Attribute Msg
frameStyle pos size tile =
  style
    ([ "position" => "absolute"
    , "left" => (toString pos.x ++ "px")
    , "top" => (toString pos.y ++ "px")
    , "width" => (toString size.width ++ "px")
    , "height" => (toString size.height ++ "px")
    , "vertical-align" => "top"
    ] ++ if tile == None then ["background-color" => "#fff"] else [])
