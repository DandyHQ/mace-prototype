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
  let
    resized = Frame.resize model.resizeDrag model.frames
    hovered = Frame.hover model.moveDrag resized
    dragDirection =
      case model.resizeDrag of
        Nothing -> Nothing
        Just d ->
          Just d.frame.tile
  in
  div [ pageStyle dragDirection ]
    [ frame hovered
    , floatingTab model.moveDrag
    ]

{-| maps a frame structure into a list of rendered windows -}
frame : Frame -> Html Msg
frame f =
  let
    children_ l =
      case l of
        [] -> []
        hd :: [] ->
          frame_ hd
        hd :: tl ->
          frame_ hd ++ border hd ++ children_ tl
    frame_ f =
      case f.children of
        FrameFrame l ->
          children_ l
        WindowFrame w ->
          window f.size f.pos w
  in
  div [] (frame_ f)

{-| renders a single window. which contains tabs and input space -}
window : Size -> Position -> Window -> List (Html Msg)
window size pos w =
  [ div [ windowStyle size pos ]
      [ tabBar size pos w
      , div [ windowShadow w.shadow (Size size.width (size.height - 35)) (Position 0 35) ] []
      ]
  ]

{-| renders the tab bar -}
tabBar : Size -> Position -> Window -> Html Msg
tabBar size pos w =
  let
    tabSize =
      let average = size.width // List.length w.tabs in
      if average > 200 then
        Size 200 30
      else
        Size average 30
    tab_ i shift l =
      case l of
        [] -> []
        hd :: tl ->
          div
            [ tabStyle (i == w.focused) tabSize shift
            , onMouseDownTab hd (Position (pos.x + shift.x) pos.y)
            ]
            [ div [ rearrangeIndicator (Just i == w.hover) (Position 0 0) ] []
            , text hd.path
            ]
          :: tab_ (i + 1) (Position (shift.x + tabSize.width) 0) tl
  in
  div [ tabBarStyle ]
    ( tab_ 0 (Position 0 0) w.tabs
    ++ [div [
          rearrangeIndicator
            (case w.hover of
              Nothing -> False
              Just i ->
                i > List.length w.tabs - 1
            )
            (Position (List.length w.tabs * tabSize.width) 5)
        ] []]
    )

{-| places a border between frames which can be used to resize them -}
border : Frame -> List (Html Msg)
border frame =
  [ div
      [ borderStyle frame.tile frame.size frame.pos
      , onMouseDownBorder frame ]
      []
  ]

-- tabBar : Maybe MoveDrag -> WindowPositioned -> Html Msg
-- tabBar drag w =
--     case w of
--       WindowPos id pos size shadow focused l ->
--         let
--           tabWidth =
--             if size.width // List.length l > 200 then
--               200
--             else
--               size.width // List.length l
--           barWidth =
--             if tabWidth * List.length l < size.width - 2 then
--               tabWidth * List.length l
--             else
--               size.width - 2
--           tabOverlaps pos =
--             case drag of
--               Nothing -> False
--               Just d ->
--                 d.moved
--                 && abs (pos.y - (d.current.y + d.offset.y)) < 15
--                 && abs (pos.x - (d.current.x + d.offset.x)) < tabWidth // 2
--           window_ i rem l =
--             case l of
--               [] -> []
--               hd :: [] ->
--                 case hd of
--                   Tab id name contents ->
--                     div [ style (tabStyle (focused == i) rem (barWidth - rem)), onMouseDownWindow hd (Position (pos.x + tabWidth * i) pos.y) ] [ div [ rearrangeIndicator (tabOverlaps (Position (pos.x + tabWidth * i) pos.y)) ] [], text (FilePath.takeFileName name) ] :: []
--               hd :: tl ->
--                 case hd of
--                   Tab id name contents ->
--                     div [ style (tabStyle (focused == i) tabWidth (barWidth - rem)), onMouseDownWindow hd (Position (pos.x + tabWidth * i) pos.y) ] [ div [ rearrangeIndicator (tabOverlaps (Position (pos.x + tabWidth * i) pos.y)) ] [], text (FilePath.takeFileName name) ] :: window_ (i+1) (rem - tabWidth) tl
--         in
--         div [ tabBarStyle ] (window_ 0 barWidth l)

-- view : Model -> Html Msg
-- view model =
--   div [ pageStyle ]
--     (
--       let
--         windowList = Frame.layoutWindows model.window model.frames
--         windowList_ = Frame.tabShadow model.moveDrag windowList
--       in List.concatMap (window model.moveDrag) windowList_
--     ++ [ windowDrag model.moveDrag ])
--
-- window : Maybe MoveDrag -> WindowPositioned -> List (Html Msg)
-- window drag w =
--   case w of
--     WindowPos id pos size shadow focused l ->
--       let
--         tabShadow =
--           case shadow of
--             NoShadow -> div [] []
--             Top ->
--               div
--                 [ style
--                     [ "position" => "absolute"
--                     , "top" => "35px"
--                     , "width" => (toString size.width ++ "px")
--                     , "height" => (toString (size.height // 2) ++ "px")
--                     , "background-color" => "rgba(0, 0, 0, 0.05)"
--                     ]
--                 ]
--                 []
--             Right ->
--               div
--                 [ style
--                     [ "position" => "absolute"
--                     , "top" => "35px"
--                     , "right" => "0"
--                     , "width" => (toString (size.width // 2) ++ "px")
--                     , "height" => (toString (size.height - 35) ++ "px")
--                     , "background-color" => "rgba(0, 0, 0, 0.05)"
--                     ]
--                 ]
--                 []
--             Bottom ->
--               div
--                 [ style
--                     [ "position" => "absolute"
--                     , "bottom" => "0"
--                     , "width" => (toString size.width ++ "px")
--                     , "height" => (toString (size.height // 2 - 35) ++ "px")
--                     , "background-color" => "rgba(0, 0, 0, 0.05)"
--                     ]
--                 ]
--                 []
--             Left ->
--               div
--                 [ style
--                     [ "position" => "absolute"
--                     , "top" => "35px"
--                     , "left" => "0"
--                     , "width" => (toString (size.width // 2) ++ "px")
--                     , "height" => (toString (size.height - 35) ++ "px")
--                     , "background-color" => "rgba(0, 0, 0, 0.05)"
--                     ]
--                 ]
--                 []
--             Center ->
--               div
--                 [ style
--                     [ "position" => "absolute"
--                     , "top" => "35px"
--                     , "left" => "0"
--                     , "width" => (toString size.width ++ "px")
--                     , "height" => (toString (size.height - 35) ++ "px")
--                     , "background-color" => "rgba(0, 0, 0, 0.05)"
--                     ]
--                 ]
--                 []
--       in
--       [div [ windowStyle pos size ]
--         ([ tabBar drag w, tabShadow ]
--         ++ case Array.get focused (Array.fromList l) of
--           Nothing -> [ div [] [] ]
--           Just w ->
--             case w of
--               Tab id name contents ->
--                 [ div [ commandBarStyle ] [ text (name ++ " New Cut Snarf Paste Eval") ]
--                 , textarea  [ inputStyle size ] [ text contents ]
--                 ]
--         )
--       ]
--
-- tabBar : Maybe MoveDrag -> WindowPositioned -> Html Msg
-- tabBar drag w =
--     case w of
--       WindowPos id pos size shadow focused l ->
--         let
--           tabWidth =
--             if size.width // List.length l > 200 then
--               200
--             else
--               size.width // List.length l
--           barWidth =
--             if tabWidth * List.length l < size.width - 2 then
--               tabWidth * List.length l
--             else
--               size.width - 2
--           tabOverlaps pos =
--             case drag of
--               Nothing -> False
--               Just d ->
--                 d.moved
--                 && abs (pos.y - (d.current.y + d.offset.y)) < 15
--                 && abs (pos.x - (d.current.x + d.offset.x)) < tabWidth // 2
--           window_ i rem l =
--             case l of
--               [] -> []
--               hd :: [] ->
--                 case hd of
--                   Tab id name contents ->
--                     div [ style (tabStyle (focused == i) rem (barWidth - rem)), onMouseDownWindow hd (Position (pos.x + tabWidth * i) pos.y) ] [ div [ rearrangeIndicator (tabOverlaps (Position (pos.x + tabWidth * i) pos.y)) ] [], text (FilePath.takeFileName name) ] :: []
--               hd :: tl ->
--                 case hd of
--                   Tab id name contents ->
--                     div [ style (tabStyle (focused == i) tabWidth (barWidth - rem)), onMouseDownWindow hd (Position (pos.x + tabWidth * i) pos.y) ] [ div [ rearrangeIndicator (tabOverlaps (Position (pos.x + tabWidth * i) pos.y)) ] [], text (FilePath.takeFileName name) ] :: window_ (i+1) (rem - tabWidth) tl
--         in
--         div [ tabBarStyle ] (window_ 0 barWidth l)
--
--
floatingTab : Maybe MoveDrag -> Html Msg
floatingTab drag =
  case drag of
    Nothing -> div [] []
    Just d ->
      if d.moved then
        div [ tabStyle True (Size 200 30) (Position (d.current.x + d.offset.x) (d.current.y + d.offset.y)) ]
          [ text d.tab.path ]
      else
        div [] []

-- HELPERS

windowShadow : Shadow -> Size -> Position -> Attribute Msg
windowShadow shadow size pos =
  if shadow == NoShadow then style [] else
    style
      [ "position" => "absolute"
      , "top" => (toString (if shadow == Bottom then pos.y + (size.height // 2) else pos.y) ++ "px")
      , "left" => (toString (if shadow == Right then pos.x + (size.width // 2) else pos.x) ++ "px")
      , "width" => (toString (if shadow == Left || shadow == Right then (size.width - size.width // 2) else size.width) ++ "px")
      , "height" => (toString (if shadow == Top || shadow == Bottom then (size.height - size.height // 2) else size.height) ++ "px")
      , "background-color" => "rgba(0, 0, 0, 0.1)"
      ]

onMouseDownTab : Tab -> Position -> Attribute Msg
onMouseDownTab w tabPos =
  on "mousedown" (Decode.map (Msg.MoveStart w tabPos) Mouse.position)

onMouseDownBorder : Frame -> Attribute Msg
onMouseDownBorder f =
  on "mousedown" (Decode.map (Msg.ResizeStart f) Mouse.position)

-- STYLES

rearrangeIndicator : Bool -> Position -> Attribute Msg
rearrangeIndicator hovered pos =
  if hovered then
    style
      [ "position" => "absolute"
      , "left" => (toString pos.x ++ "px")
      , "top" => (toString pos.y ++ "px")
      , "height" => "30px"
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
--
-- commandBarStyle : Attribute Msg
-- commandBarStyle =
--   style
--     [ "height" => "25px"
--     , "line-height" => "25px"
--     , "background-color" => "#f1f1f1"
--     , "color" => "#4c4c4c"
--     ]
--
-- inputStyle : Size -> Attribute Msg
-- inputStyle size =
--   style
--     [ "width" => (toString size.width ++ "px")
--     , "height" => (toString (size.height - 36 - 25) ++ "px")
--     , "border" => "0"
--     , "margin" => "0"
--     , "padding" => "0"
--     , "overflow" => "auto"
--     , "resize" => "none"
--     ]
--
tabStyle : Bool -> Size -> Position -> Attribute Msg
tabStyle focused size pos =
  style ([ "position" => "absolute"
    , "top" => (toString pos.y ++ "px")
    , "left" => (toString pos.x ++ "px")
    , "border-bottom" => if focused then "1px solid #f1f1f1" else "1px solid #c6c6c6"
    , "margin-top" => "5px"
    , "display" => "inline-block"
    , "width" => (toString (size.width - (if focused then 2 else 0)) ++ "px")
    , "height" => (toString size.height ++ "px")
    , "line-height" => "30px"
    , "text-align" => "center"
    , "color" => if focused then "#4c4c4c" else "#818181"
    , "background-color" => if focused then "#f1f1f1" else "#e7e7e7"
    , "cursor" => "default"
    , "overflow" => "hidden"
    ]
    ++
    if focused then
      [ "border-top" => "1px solid #c6c6c6"
      , "border-right" => "1px solid #c6c6c6"
      , "border-left" => "1px solid #c6c6c6"
      , "border-radius" => "5px 5px 0 0"
      ]
    else
      [])

borderStyle : Tile -> Size -> Position -> Attribute Msg
borderStyle tile size pos =
  style
    [ "position" => "absolute"
    , "left" => (toString (if tile == Horiz then pos.x + size.width - 1 else pos.x) ++ "px")
    , "top" => (toString (if tile == Horiz then pos.y else pos.y + size.height - 1) ++ "px")
    , "width" => (toString (if tile == Horiz then 3 else size.width) ++ "px")
    , "height" => (toString (if tile == Horiz then size.height else 3) ++ "px")
    , "cursor" => if tile == Horiz then "ew-resize" else "ns-resize"
    , "z-index" => "10"
    ]

pageStyle : Maybe Tile -> Attribute Msg
pageStyle dragDirection =
  style
    [ "width" => "100%"
    , "height" => "100%"
    , "background-color" => "#c6c6c6"
    , "cursor" => (case dragDirection of
        Just Horiz -> "ew-resize"
        Just Vert -> "ns-resize"
        _ -> "default")
    , "-webkit-touch-callout" => "none"
    , "-webkit-user-select" => "none"
    , "-khtml-user-select" => "none"
    , "-moz-user-select" => "none"
    , "-ms-user-select" => "none"
    , "user-select" => "none"
    ]

windowStyle : Size -> Position -> Attribute Msg
windowStyle size pos =
  style
    [ "width" => (toString size.width ++ "px")
    , "height" => (toString size.height ++ "px")
    , "background-color" => "white"
    , "position" => "absolute"
    , "top" => (toString pos.y ++ "px")
    , "left" => (toString pos.x ++ "px")
    ]
