module Frame exposing (initial, resizeAll, focus, layoutWindows, tabShadow, applySplit)

import Types exposing (..)

{- initial test layout of the application window -}
initial : Frame
initial =
  Frame 0 600 Horiz ( FrameFrame
    [ Frame 1 299 Horiz ( WindowFrame 0
        [ Tab 2 "/root/tutorial2.py" "cat"
        , Tab 3 "/root/example2.py" "dog"
        ])
    , Frame 4 299 Horiz ( FrameFrame
        [ Frame 5 299 Vert
            ( WindowFrame 0 [ Tab 6 "/root/readme.md" "tiger" ] )
        , Frame 7 299 Vert ( WindowFrame 0
            [ Tab 8 "/root/mouse.c" "pidgin"
            , Tab 9 "/root/example2.py" "frog"
            , Tab 10 "/root/music.c" "song"
            ])
        ])
    ])

{- applies a function to all frames -}
map : (Frame -> Frame) -> Frame -> Frame
map fn frame =
  let
    frame_ f =
      case f of
        Frame id s t c ->
          fn (Frame id s t (case c of
            FrameFrame l ->
              FrameFrame (frameChildren_ l)
            WindowFrame id l ->
              WindowFrame id l))
    frameChildren_ l =
      case l of
        [] -> []
        hd :: tl ->
          frame_ hd :: frameChildren_ tl
  in
  frame_ frame

{-| builds a scale function and applies it to all frames -}
resizeAll : Size -> Size -> Frame -> Frame
resizeAll oldSize newSize f =
  let
    scale f =
      case f of
        Frame id s t c ->
          case t of
            Horiz -> Frame id (round (toFloat newSize.width / toFloat oldSize.width * toFloat s)) t c
            Vert -> Frame id (round (toFloat newSize.height / toFloat oldSize.height * toFloat s))t c
            _ -> f
  in
  map scale f


-- {-| resizes two frames according to the drag on their border -}
-- resize : Maybe ResizeDrag -> Frame -> Frame
-- resize drag frame =
--   let
--     resize_ d t l =
--       case l of
--         [] -> []
--         hd :: [] -> resizeFrame_ d hd :: []
--         a :: b :: tl ->
--           if a == d.frame then
--             -- we have found the frame. now do the resize
--             case t of
--               Horiz ->
--                 case a of
--                   Frame s t c ->
--                     Frame (s + d.current.x - d.start.x) t c
--                 ::
--                 case b of
--                   Frame s t c ->
--                     Frame (s - (d.current.x - d.start.x)) t c
--                 :: tl
--               Vert ->
--                 case a of
--                   Frame s t c ->
--                     Frame (s + d.current.y - d.start.y) t c
--                 ::
--                 case b of
--                   Frame s t c ->
--                     Frame (s - (d.current.y - d.start.y)) t c
--                 :: tl
--               -- something's wrong
--               _ ->
--                 a :: b :: tl
--           else
--             a :: resize_ d t (b :: tl)
--
--     resizeFrame_ d f =
--       case f of
--         Frame s t c ->
--           case c of
--             FrameFrame l ->
--               Frame s t (FrameFrame (resize_ d t l))
--             WindowFrame _ _ ->
--               f
--   in
--   case drag of
--     Nothing -> frame
--     Just d ->
--       resizeFrame_ d frame


{-| focuses a window from the current frame group -}
focus : Tab -> Frame -> Frame
focus window frame =
  let
    frameChildren_ l =
      List.indexedMap frame_ l
    frame_ i f =
      case f of
        Frame id s t c ->
          case c of
            FrameFrame l ->
              Frame id s t (FrameFrame (frameChildren_ l))
            WindowFrame _ l ->
              let
                focused = List.sum (List.indexedMap (\k v -> if v == window then k + 1 else 0) l)
              in
              if focused /= 0 then
                Frame id s t (WindowFrame (focused - 1) l)
              else
                f
  in
    frame_ 0 frame


{-| mutually recursive with positionFrame to perform a window layout -}
positionFrameChildren : Position -> Size -> Size -> List Frame -> List WindowPositioned
positionFrameChildren pos parentSize rem l =
  let
    pos_ = Position (pos.x + parentSize.width - rem.width) (pos.y + parentSize.height - rem.height)
  in
  case l of
    [] -> []
    hd :: [] ->
      positionFrame pos_ rem hd
    hd :: tl ->
      let
        (s, tile) =
          case hd of
            Frame _ s t _ -> (s, t)
        size =
          case tile of
            Horiz -> Size (parentSize.width - s - 1) parentSize.height
            Vert -> Size parentSize.width (parentSize.height - s - 1)
            NoTile -> parentSize
        rem_ =
          case tile of
            Horiz -> Size (rem.width - size.width - 1) rem.height
            Vert -> Size rem.width (rem.height - size.height - 1)
            NoTile -> rem
      in
      positionFrame pos_ size hd
      ++ positionFrameChildren pos parentSize rem_ tl

{-| mutually recursive with positionFrameChildren to perform a window layout -}
positionFrame : Position -> Size -> Frame -> List WindowPositioned
positionFrame pos size f =
  case f of
    Frame id s t c ->
      case c of
        FrameFrame l ->
          positionFrameChildren pos size size l
        WindowFrame focused l ->
          WindowPos id pos size NoShadow focused l :: []

{-| lays the windows out, positioned absolutely within the size (assumes they have been resized first) -}
layoutWindows : Size -> Frame -> List WindowPositioned
layoutWindows size frame =
  positionFrame (Position 0 0) size frame

{-| figures out whether a window should be divided if the hovering tab is dropped -}
tabShadow : Maybe MoveDrag -> List WindowPositioned -> List WindowPositioned
tabShadow drag windowList =
  let
    windowShadowed d w =
      case w of
        WindowPos id pos size _ focused l ->
          -- top
          if (d.current.y + d.offset.y) > pos.y + 35 && (d.current.y + d.offset.y) < pos.y + (size.height - 35) // 2
            && (d.current.x + d.offset.x) > pos.x + 100 && (d.current.x + d.offset.x) < pos.x + size.width - 300
            then
            WindowPos id pos size Top focused l
          -- right
          else if (d.current.y + d.offset.y) > pos.y + 35 && (d.current.y + d.offset.y) < pos.y + size.height
            && (d.current.x + d.offset.x) > pos.x + size.width - 300 && (d.current.x + d.offset.x) < pos.x + size.width - 100
            then
            WindowPos id pos size Right focused l
          -- bottom
          else if (d.current.y + d.offset.y) > pos.y + 35 + size.height // 2 && (d.current.y + d.offset.y) < pos.y + size.height
            && (d.current.x + d.offset.x) > pos.x + 100 && (d.current.x + d.offset.x) < pos.x + size.width - 300
            then
            WindowPos id pos size Bottom focused l
          -- left
          else if (d.current.y + d.offset.y) > pos.y + 35 && (d.current.y + d.offset.y) < pos.y + size.height
            && (d.current.x + d.offset.x) > pos.x - 100 && (d.current.x + d.offset.x) < pos.x + 100
            then
            WindowPos id pos size Left focused l
          else if (d.current.y + d.offset.y) > pos.y + 35 && (d.current.y + d.offset.y) < pos.y + size.height
            && (d.current.x + d.offset.x) > pos.x && (d.current.x + d.offset.x) < pos.x + size.width - 100
            then
            WindowPos id pos size Center focused l
          else
            w
  in
  case drag of
    Nothing -> windowList
    Just d -> List.map (windowShadowed d) windowList

{-| find the dragged tab and moves it into the frame that is being hovered over -}
applySplit : Maybe MoveDrag -> List WindowPositioned -> Frame -> Frame
applySplit drag windowList frame =
  let
    visitFrameChildren d w l =
      case l of
        [] -> []
        hd :: tl ->
          visitFrame d w hd :: visitFrameChildren d w tl
    visitTabChildren d w l =
      case l of
        [] -> []
        hd :: tl ->
          -- remove the tab
          if hd == d.window then
            visitTabChildren d w tl
          else
            hd :: visitTabChildren d w tl
    visitFrame d w f =
      let id2 = case w of WindowPos id _ _ _ _ _ -> id in
      case f of
        Frame id size tile c ->
          case c of
            FrameFrame l ->
              Frame id size tile (FrameFrame (visitFrameChildren d w l))
            WindowFrame focus l ->
              let visited = visitTabChildren d w l in
              if id == id2 && visited /= l then f
              else if id == id2 then
                Frame id size tile (WindowFrame focus (visited ++ [d.window]))
              else
                Frame id size tile (WindowFrame focus visited)
  in
  case drag of
    Nothing -> frame
    Just d ->
      case List.filter (\k -> case k of WindowPos _ _ _ shadow _ _ -> shadow /= NoShadow) windowList of
        [] -> frame
        hd :: _ ->  visitFrame d hd frame
