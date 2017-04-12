module View exposing (view)

import Array
import FilePath
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
    (
      let
        windowList = Frame.layoutWindows model.window model.frames
        windowList_ = Frame.tabShadow model.moveDrag windowList
      in List.concatMap (window model.moveDrag) windowList_
    ++ [ windowDrag model.moveDrag ])

window : Maybe MoveDrag -> WindowPositioned -> List (Html Msg)
window drag w =
  case w of
    WindowPos id pos size shadow focused l ->
      let
        tabShadow =
          case shadow of
            NoShadow -> div [] []
            Top ->
              div
                [ style
                    [ "position" => "absolute"
                    , "top" => "35px"
                    , "width" => (toString size.width ++ "px")
                    , "height" => (toString (size.height // 2) ++ "px")
                    , "background-color" => "rgba(0, 0, 0, 0.05)"
                    ]
                ]
                []
            Right ->
              div
                [ style
                    [ "position" => "absolute"
                    , "top" => "35px"
                    , "right" => "0"
                    , "width" => (toString (size.width // 2) ++ "px")
                    , "height" => (toString (size.height - 35) ++ "px")
                    , "background-color" => "rgba(0, 0, 0, 0.05)"
                    ]
                ]
                []
            Bottom ->
              div
                [ style
                    [ "position" => "absolute"
                    , "bottom" => "0"
                    , "width" => (toString size.width ++ "px")
                    , "height" => (toString (size.height // 2 - 35) ++ "px")
                    , "background-color" => "rgba(0, 0, 0, 0.05)"
                    ]
                ]
                []
            Left ->
              div
                [ style
                    [ "position" => "absolute"
                    , "top" => "35px"
                    , "left" => "0"
                    , "width" => (toString (size.width // 2) ++ "px")
                    , "height" => (toString (size.height - 35) ++ "px")
                    , "background-color" => "rgba(0, 0, 0, 0.05)"
                    ]
                ]
                []
            Center ->
              div
                [ style
                    [ "position" => "absolute"
                    , "top" => "35px"
                    , "left" => "0"
                    , "width" => (toString size.width ++ "px")
                    , "height" => (toString (size.height - 35) ++ "px")
                    , "background-color" => "rgba(0, 0, 0, 0.05)"
                    ]
                ]
                []
      in
      [div [ windowStyle pos size ]
        ([ tabBar drag w, tabShadow ]
        ++ case Array.get focused (Array.fromList l) of
          Nothing -> [ div [] [] ]
          Just w ->
            case w of
              Tab id name contents ->
                [ div [ commandBarStyle ] [ text (name ++ " New Cut Snarf Paste Eval") ]
                , textarea  [ inputStyle size ] [ text contents ]
                ]
        )
      ]

tabBar : Maybe MoveDrag -> WindowPositioned -> Html Msg
tabBar drag w =
    case w of
      WindowPos id pos size shadow focused l ->
        let
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
          tabOverlaps pos =
            case drag of
              Nothing -> False
              Just d ->
                d.moved
                && abs (pos.y - (d.current.y + d.offset.y)) < 15
                && abs (pos.x - (d.current.x + d.offset.x)) < tabWidth // 2
          window_ i rem l =
            case l of
              [] -> []
              hd :: [] ->
                case hd of
                  Tab id name contents ->
                    div [ style (tabStyle (focused == i) rem (barWidth - rem)), onMouseDownWindow hd (Position (pos.x + tabWidth * i) pos.y) ] [ div [ rearrangeIndicator (tabOverlaps (Position (pos.x + tabWidth * i) pos.y)) ] [], text (FilePath.takeFileName name) ] :: []
              hd :: tl ->
                case hd of
                  Tab id name contents ->
                    div [ style (tabStyle (focused == i) tabWidth (barWidth - rem)), onMouseDownWindow hd (Position (pos.x + tabWidth * i) pos.y) ] [ div [ rearrangeIndicator (tabOverlaps (Position (pos.x + tabWidth * i) pos.y)) ] [], text (FilePath.takeFileName name) ] :: window_ (i+1) (rem - tabWidth) tl
        in
        div [ tabBarStyle ] (window_ 0 barWidth l)


windowDrag : Maybe MoveDrag -> Html Msg
windowDrag drag =
  case drag of
    Nothing -> div [] []
    Just d ->
      if d.moved then
        div [ style (tabStyle True 200 (d.current.x + d.offset.x) ++ ["top" => (toString (d.current.y + d.offset.y) ++ "px")]) ]
          [ text (case d.window of Tab _ name _ -> FilePath.takeFileName name) ]
      else
        div [] []

-- HELPERS

onMouseDownWindow : Tab -> Position -> Attribute Msg
onMouseDownWindow w tabPos =
  on "mousedown" (Decode.map (Msg.MoveStart w tabPos) Mouse.position)

onMouseDownFrame : Frame -> Attribute Msg
onMouseDownFrame f =
  on "mousedown" (Decode.map (Msg.ResizeStart f) Mouse.position)

-- STYLES

rearrangeIndicator : Bool -> Attribute Msg
rearrangeIndicator overlap =
  if overlap then
    style
      [ "position" => "absolute"
      , "left" => "0"
      , "height" => "100%"
      , "width" => "3px"
      , "background-color" => "#4e8dbd"
      ]
  else
    style []

tabBarStyle : Attribute Msg
tabBarStyle =
  style
    [ "height" => "35px"
    , "background-color" => "#e7e7e7"
    , "border-bottom" => "1px solid #c6c6c6"
    ]

commandBarStyle : Attribute Msg
commandBarStyle =
  style
    [ "height" => "25px"
    , "line-height" => "25px"
    , "background-color" => "#f1f1f1"
    , "color" => "#4c4c4c"
    ]

inputStyle : Size -> Attribute Msg
inputStyle size =
  style
    [ "width" => (toString size.width ++ "px")
    , "height" => (toString (size.height - 36 - 25) ++ "px")
    , "border" => "0"
    , "margin" => "0"
    , "padding" => "0"
    , "overflow" => "auto"
    , "resize" => "none"
    ]

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
