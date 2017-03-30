module View exposing (view)

import Array
import Frame
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, on)
import Json.Decode as Decode
import Model exposing (Model)
import Mouse
import Msg exposing (Msg)
import Types exposing (..)

(=>) =
  (,)

view : Model -> Html Msg
view model =
  div [ style [ "width" => "100%", "height" => "100%", "background-color" => "#c6c6c6" ] ]
    [ frame (Position 0 0) model.window None (Frame.resize model.drag model.frames) ]

frameChildren : Size -> Size -> Tile -> List Frame -> List (Html Msg)
frameChildren size rem tile l =
  case l of
    [] -> []
    hd :: [] ->
      frame (Position (size.width - rem.width) (size.height - rem.height)) rem tile hd :: []
    hd :: tl ->
      frame (Position (size.width - rem.width) (size.height - rem.height)) rem tile hd
        -- add the border in
        :: div [ onMouseDown hd, borderStyle (pos_of_size (getSize (Size (size.width - rem.width) (size.height - rem.height)) tile hd)) tile ] []
        -- and the remaining frames
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
            window l
        )

window : List Window -> List (Html Msg)
window l =
  let
    window_ l =
      case l of
        [] -> []
        hd :: tl ->
          case hd of
            Window id focused visible ->
              div [ tabStyle visible] [ text ("Window " ++ toString id) ] :: window_ tl
  in
  [div []
    [ div [ style ["background-color" => "#e7e7e7"] ] (window_ l)
    , div [ style ["background-color" => "#f1f1f1", "color" => "#4c4c4c"] ] [ text "New Cut Snarf Paste Eval" ]
    , div [] []
    ]
  ]


-- HELPERS

onMouseDown : Frame -> Attribute Msg
onMouseDown f =
  on "mousedown" (Decode.map (Msg.DragStart f) Mouse.position)

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

pos_of_size : Size -> Position
pos_of_size size =
  Position size.width size.height


-- STYLES

tabStyle : Bool -> Attribute Msg
tabStyle visible =
  style
    ([ "border-bottom" => if visible then "none" else "1px solid #c6c6c6"
    , "margin-top" => "5px"
    , "display" => "inline-block"
    , "width" => "200px"
    , "height" => "30px"
    , "line-height" => "30px"
    , "text-align" => "center"
    , "color" => if visible then "#4c4c4c" else "#818181"
    , "background-color" => if visible then "#f1f1f1" else "#e7e7e7"
    ] ++
      if visible then
        [ "border-top" => "1px solid #c6c6c6"
        , "border-right" => "1px solid #c6c6c6"
        , "border-left" => "1px solid #c6c6c6"
        , "border-radius" => "5px 5px 0 0"
        ]
      else
        [])

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

borderStyle : Position -> Tile -> Attribute Msg
borderStyle pos tile =
  style
    [ "position" => "absolute"
    , "left" => (toString (pos.x - if tile == Horiz then 1 else 0) ++ "px")
    , "top" => (toString (pos.y - if tile == Vert then 1 else 0) ++ "px")
    , "width" => if tile == Horiz then "3px" else "100%"
    , "height" => if tile == Vert then "3px" else "100%"
    , "cursor" => if tile == Horiz then "ew-resize" else "ns-resize"
    ]
