module View exposing (view)

import Array
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, on)
import Model exposing (Model)
import Mouse exposing (Position)
import Msg exposing (Msg)
import Types exposing (..)

(=>) =
  (,)

view : Model -> Html Msg
view model =
  div [ style [ "width" => "100%", "height" => "100%", "background-color" => "#aaa" ] ]
    [ frame None model.frames ]

frameChildren : Int -> Int -> Tile -> FrameChildren -> List (Html Msg)
frameChildren w h t f =
  let
    divideFrame l =
      case l of
        [] -> []
        hd :: [] ->
          frame t hd :: []
        a :: b :: tl ->
          case b of
            Frame _ o _ _ ->
              frame t a
                ::
                  (case t of
                    Horiz -> div [ onMouseDown, style [ "z-index" => "1000", "position" => "absolute", "width" => "2px", "height" => "100%", "background-color" => "#ff0", "cursor" => "ew-resize", "left" => (toString (o - 2) ++ "px") ] ] []
                    Vert -> div [ onMouseDown, style [ "z-index" => "1000", "position" => "absolute", "width" => "100%", "height" => "2px", "background-color" => "#ff0", "cursor" => "ns-resize", "top" => (toString (o - 2) ++ "px") ] ] []
                    None -> div [] []
                  )
                :: divideFrame (b :: tl)
  in
  case f of
    FrameFrame l ->
      divideFrame l
    WindowFrame l ->
      List.map (window w h) l

frame : Tile -> Frame -> Html Msg
frame t f =
  case f of
    Frame {width, height} o Horiz c ->
      div [ frameStyle width height t o "row" [] ]
        (frameChildren width height Horiz c)
        -- ( List.intersperse
        --     (div [ style [ "display" => "inline-block", "width" => "2px", "background" => "#ff0", "cursor" => "ew-resize" ] ] [])
        --     (frameChildren width height Horiz c)
        -- )
    Frame {width, height} o Vert c ->
      div [ frameStyle width height t o "column" [] ]
        (frameChildren width height Vert c)
        -- ( List.intersperse
        --     (div [ style [ "height" => "2px", "background" => "blue", "cursor" => "ns-resize" ] ] [])
        --     (frameChildren w (h // 2) Vert c)
        -- )
    Frame {width, height} o None c ->
      div [ frameStyle width height t o "row" [ "background-color" => "#fff" ] ]
        ( frameChildren width height None c)

window : Int -> Int -> Window -> Html Msg
window w h f =
  text "w "


onMouseDown : Attribute Msg
onMouseDown =
  on "mousedown" (Decode.map Msg.DragStart Mouse.position)

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
