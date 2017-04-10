module View exposing (view)

import Array
import Frame
import Html exposing (..)
import Html.Attributes exposing (style, value)
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
  div [ pageStyle ]
    [ frame (Position 0 0) model.window None (Frame.resize model.resizeDrag model.frames)
    , windowDrag model.moveDrag ]

frameChildren : Size -> Size -> Tile -> List Frame -> List (Html Msg)
frameChildren size rem tile l =
  case l of
    [] -> []
    hd :: [] ->
      frame (Position (size.width - rem.width) (size.height - rem.height)) rem tile hd :: []
    hd :: tl ->
      frame (Position (size.width - rem.width) (size.height - rem.height)) rem tile hd
        -- add the border in
        :: div [ onMouseDownFrame hd, borderStyle (pos_of_size (getSize (Size (size.width - rem.width) (size.height - rem.height)) tile hd)) tile ] []
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
            window (getSize size tile f) l
        )

window : Size -> List Window -> List (Html Msg)
window size l =
  let
    findVisible l =
      List.head
        ( List.filter (\v -> case v of Window _ _ visible _ -> visible) l )
    window_ l =
      case l of
        [] -> []
        hd :: tl ->
          case hd of
            Window id focused visible contents ->
              div [ style (tabStyle visible), onMouseDownWindow hd ] [ text ("Window " ++ toString id) ] :: window_ tl
  in
  [div [ style ["width" => (toString size.width ++ "px"), "height" => (toString size.height ++ "px")] ]
    [ div [ style ["height" => "37px", "background-color" => "#e7e7e7"] ] (window_ l)
    , div [ style ["height" => "25px", "line-height" => "25px", "background-color" => "#f1f1f1", "color" => "#4c4c4c"] ] [ text "New Cut Snarf Paste Eval" ]
    , textarea  [ style
                    [ "width" => (toString size.width ++ "px")
                    , "height" => (toString (size.height - 37 - 25) ++ "px")
                    , "border" => "0"
                    , "margin" => "0"
                    , "padding" => "0"
                    , "overflow" => "auto"
                    , "resize" => "none"
                    ] ]
        [ text (case findVisible l of
          Nothing -> ""
          Just w ->
            case w of
              Window _ _ _ contents ->
                contents
        )]
    ]
  ]


windowDrag : Maybe MoveDrag -> Html Msg
windowDrag drag =
  case drag of
    Nothing -> div [] []
    Just d ->
      div [ style (tabStyle True ++ [ "position" => "absolute", "top" => (toString d.current.y ++ "px"), "left" => (toString d.current.x ++ "px") ]) ]
        [ text ("Window " ++ (case d.window of Window id _ _ _ -> toString id)) ]

-- HELPERS

onMouseDownWindow : Window -> Attribute Msg
onMouseDownWindow w =
  on "mousedown" (Decode.map (Msg.MoveStart w) Mouse.position)

onMouseDownFrame : Frame -> Attribute Msg
onMouseDownFrame f =
  on "mousedown" (Decode.map (Msg.ResizeStart f) Mouse.position)

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

tabStyle : Bool -> List (String, String)
tabStyle visible =
    [ "border-bottom" => if visible then "1px solid #f1f1f1" else "1px solid #c6c6c6"
    , "margin-top" => "5px"
    , "display" => "inline-block"
    , "width" => "200px"
    , "height" => "30px"
    , "line-height" => "30px"
    , "text-align" => "center"
    , "color" => if visible then "#4c4c4c" else "#818181"
    , "background-color" => if visible then "#f1f1f1" else "#e7e7e7"
    , "cursor" => "default"
    ]
    ++
    if visible then
      [ "border-top" => "1px solid #c6c6c6"
      , "border-right" => "1px solid #c6c6c6"
      , "border-left" => "1px solid #c6c6c6"
      , "border-radius" => "5px 5px 0 0"
      ]
    else
      []

frameStyle : Position -> Size -> Tile -> Attribute Msg
frameStyle pos size tile =
  style
    [ "position" => "absolute"
    , "left" => (toString pos.x ++ "px")
    , "top" => (toString pos.y ++ "px")
    , "width" => (toString size.width ++ "px")
    , "height" => (toString size.height ++ "px")
    , "vertical-align" => "top"
    , "background-color" => if tile == None then "#fff" else "#c6c6c6"
    ]

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

pageStyle : Attribute Msg
pageStyle =
  style
    [ "width" => "100%"
    , "height" => "100%"
    , "-webkit-touch-callout" => "none"
    , "-webkit-user-select" => "none"
    , "-khtml-user-select" => "none"
    , "-moz-user-select" => "none"
    , "-ms-user-select" => "none"
    , "user-select" => "none"
    ]
