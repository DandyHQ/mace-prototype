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
    ( List.concatMap window (Frame.layoutWindows model.window (Frame.resize model.resizeDrag model.frames))
    ++ [ windowDrag model.moveDrag ])

window : WindowPositioned -> List (Html Msg)
window w =
  case w of
    WindowPos pos size l ->
      let
        findVisible l =
          List.head
            ( List.filter (\v -> case v of Window _ _ visible _ -> visible) l )
        tabWidth =
          if size.width // List.length l > 200 then
            200
          else
            size.width // List.length l
        barWidth =
          if tabWidth * List.length l < size.width - 2 then
            tabWidth * List.length l
          else
            size.width - 2
        window_ rem l =
          case l of
            [] -> []
            hd :: [] ->
              case hd of
                Window id focused visible contents ->
                  div [ style (tabStyle visible rem (barWidth - rem)), onMouseDownWindow hd ] [ text ("Window " ++ toString id) ] :: []
            hd :: tl ->
              case hd of
                Window id focused visible contents ->
                  div [ style (tabStyle visible tabWidth (barWidth - rem)), onMouseDownWindow hd ] [ text ("Window " ++ toString id) ] :: window_ (rem - tabWidth) tl
      in
      [div [ windowStyle pos size ]
        [ div [ style ["height" => "35px", "background-color" => "#e7e7e7", "border-bottom" => "1px solid #c6c6c6"] ] (window_ barWidth l)
        , div [ style ["height" => "25px", "line-height" => "25px", "background-color" => "#f1f1f1", "color" => "#4c4c4c"] ] [ text "New Cut Snarf Paste Eval" ]
        , textarea  [ style
                        [ "width" => (toString size.width ++ "px")
                        , "height" => (toString (size.height - 36 - 25) ++ "px")
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
      if d.moved then
        div [ style (tabStyle True 200 d.current.x ++ ["top" => (toString d.current.y ++ "px")]) ]
          [ text ("Window " ++ (case d.window of Window id _ _ _ -> toString id)) ]
      else
        div [] []

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

tabStyle : Bool -> Int -> Int -> List (String, String)
tabStyle visible width offset =
    [ "position" => "absolute"
    , "top" => "0"
    , "left" => (toString offset ++ "px")
    , "border-bottom" => if visible then "1px solid #f1f1f1" else "1px solid #c6c6c6"
    , "margin-top" => "5px"
    , "display" => "inline-block"
    , "width" => (toString (width - (if visible then 2 else 0)) ++ "px")
    , "height" => "30px"
    , "line-height" => "30px"
    , "text-align" => "center"
    , "color" => if visible then "#4c4c4c" else "#818181"
    , "background-color" => if visible then "#f1f1f1" else "#e7e7e7"
    , "cursor" => "default"
    , "overflow" => "hidden"
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

borderStyle : Position -> Tile -> Size -> Attribute Msg
borderStyle pos tile size =
  style
    [ "position" => "absolute"
    , "left" => (toString (pos.x - if tile == Horiz then 1 else 0) ++ "px")
    , "top" => (toString (pos.y - if tile == Vert then 1 else 0) ++ "px")
    , "width" => (toString size.width ++ "px")
    , "height" => (toString size.height ++ "px")
    , "cursor" => if tile == Horiz then "ew-resize" else "ns-resize"
    ]

pageStyle : Attribute Msg
pageStyle =
  style
    [ "width" => "100%"
    , "height" => "100%"
    , "background-color" => "#c6c6c6"
    , "-webkit-touch-callout" => "none"
    , "-webkit-user-select" => "none"
    , "-khtml-user-select" => "none"
    , "-moz-user-select" => "none"
    , "-ms-user-select" => "none"
    , "user-select" => "none"
    ]

windowStyle : Position -> Size -> Attribute Msg
windowStyle pos size =
  style
    [ "width" => (toString size.width ++ "px")
    , "height" => (toString size.height ++ "px")
    , "position" => "absolute"
    , "top" => (toString pos.y ++ "px")
    , "left" => (toString pos.x ++ "px")
    ]
