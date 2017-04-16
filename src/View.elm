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
    [ frame model.frames ]

{-| maps a frame structure into a list of rendered windows -}
frame : Frame -> Html Msg
frame f =
  let
    children_ l =
      case l of
        [] -> []
        hd :: tl ->
          frame_ hd ++ children_ tl
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
      [ tabBar size pos w ]
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
            [ text hd.path ]
          :: tab_ (i + 1) (Position (shift.x + tabSize.width) 0) tl
  in
  div [ tabBarStyle ] (tab_ 0 (Position 0 0) w.tabs)

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
-- windowDrag : Maybe MoveDrag -> Html Msg
-- windowDrag drag =
--   case drag of
--     Nothing -> div [] []
--     Just d ->
--       if d.moved then
--         div [ style (tabStyle True 200 (d.current.x + d.offset.x) ++ ["top" => (toString (d.current.y + d.offset.y) ++ "px")]) ]
--           [ text (case d.window of Tab _ name _ -> FilePath.takeFileName name) ]
--       else
--         div [] []
--
-- -- HELPERS

onMouseDownTab : Tab -> Position -> Attribute Msg
onMouseDownTab w tabPos =
  on "mousedown" (Decode.map (Msg.MoveStart w tabPos) Mouse.position)
--
-- onMouseDownFrame : Frame -> Attribute Msg
-- onMouseDownFrame f =
--   on "mousedown" (Decode.map (Msg.ResizeStart f) Mouse.position)
--
-- -- STYLES
--
-- rearrangeIndicator : Bool -> Attribute Msg
-- rearrangeIndicator overlap =
--   if overlap then
--     style
--       [ "position" => "absolute"
--       , "left" => "0"
--       , "height" => "100%"
--       , "width" => "3px"
--       , "background-color" => "#4e8dbd"
--       ]
--   else
--     style []
--
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
--
-- borderStyle : Position -> Tile -> Size -> Attribute Msg
-- borderStyle pos tile size =
--   style
--     [ "position" => "absolute"
--     , "left" => (toString (pos.x - if tile == Horiz then 1 else 0) ++ "px")
--     , "top" => (toString (pos.y - if tile == Vert then 1 else 0) ++ "px")
--     , "width" => (toString size.width ++ "px")
--     , "height" => (toString size.height ++ "px")
--     , "cursor" => if tile == Horiz then "ew-resize" else "ns-resize"
--     ]
--
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
