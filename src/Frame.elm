module Frame exposing (initial, resizeAll, focus, layoutWindows, tabShadow, applySplit)

import Types exposing (..)

{- initial test layout of the application window -}
initial : Frame
initial =
  Frame "0" (Size 600 600) Horiz ( FrameFrame
    [ Frame "00" (Size 300 600) Horiz ( WindowFrame 0
        [ Tab "000" "/root/tutorial2.py" "cat"
        , Tab "001" "/root/example2.py" "dog"
        ])
    , Frame "00" (Size 299 600) Horiz ( FrameFrame
        [ Frame "000" (Size 299 300) Vert
            ( WindowFrame 0 [ Tab "0000" "/root/readme.md" "tiger" ] )
        , Frame "001" (Size 299 299) Vert ( WindowFrame 0
            [ Tab "0010" "/root/mouse.c" "pidgin"
            , Tab "0011" "/root/example2.py" "frog"
            , Tab "0012" "/root/music.c" "song"
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
resizeAll : Size -> Frame -> Frame
resizeAll newSize f =
  let oldSize = case f of Frame _ s _ _ -> s in
  let
    scale f =
      case f of
        Frame id s t c ->
          Frame id
            (Size
              (round (toFloat newSize.width / toFloat oldSize.width * toFloat s.width))
              (round (toFloat newSize.height / toFloat oldSize.height * toFloat s.height))
            ) t c
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
            Horiz -> Size (parentSize.width - s.width - 1) parentSize.height
            Vert -> Size parentSize.width (parentSize.height - s.height - 1)
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

{-| shuffles all the IDs down giving a new number to each node -}
reId : Frame -> Frame
reId frame =
  let
    tabChildren_ id i l =
      case l of
        [] -> []
        hd :: tl ->
          case hd of
            Tab _ name contents ->
              Tab (id ++ (toString i)) name contents :: tabChildren_ id (i + 1) tl
    frameChildren_ id i l =
      case l of
        [] -> []
        hd :: tl ->
          frame_ (id ++ (toString i)) hd :: frameChildren_ id (i + 1) tl
    frame_ id f =
      case f of
        Frame _ size tile children ->
          case children of
            FrameFrame list ->
              Frame id size tile (FrameFrame (frameChildren_ id 0 list))
            WindowFrame focus list ->
              Frame id size tile (WindowFrame focus (tabChildren_ id 0 list))
  in
  frame_ "0" frame


{-| ensures no windows are focusing on non-existant tabs -}
refocusTabs : Frame -> Frame
refocusTabs frame =
    let
      frameChildren_ l =
        case l of
          [] -> []
          hd :: tl ->
            frame_ hd :: frameChildren_ tl
      frame_ f =
        case f of
          Frame id size tile children ->
            case children of
              FrameFrame list ->
                Frame id size tile (FrameFrame (frameChildren_ list))
              WindowFrame focus list ->
                Frame id size tile (WindowFrame
                  (if focus < List.length list - 1 then
                    focus
                  else
                    List.length list - 1)
                  list)
    in
    frame_ frame

{-| removes any empty frames merging them with neighbours -}
snuffEmpty : Frame -> Frame
snuffEmpty frame =
    let
      -- remove any empty windows
      windowFrameChildren_ l =
        case l of
          [] -> []
          hd :: tl ->
            case windowFrame_ hd of
              Nothing -> windowFrameChildren_ tl
              Just f -> f :: windowFrameChildren_ tl
      windowFrame_ f =
        case f of
          Frame id size tile children ->
            case children of
              FrameFrame list ->
                Just (Frame id size tile (FrameFrame (windowFrameChildren_ list)))
              WindowFrame focus list ->
                if List.length list == 0 then
                  Nothing
                else
                  Just (Frame id size tile (WindowFrame focus list))
      frameChildren_ l =
        case l of
          [] -> []
          hd :: tl -> frame_ hd :: frameChildren_ tl
      frame_ f =
        case f of
          Frame id size tile children ->
            case children of
              FrameFrame list ->
                if List.length list == 1 then
                  case Maybe.withDefault f (List.head list) of
                    Frame _ _ _ children ->
                      case children of
                        FrameFrame list ->
                          Frame id size tile (FrameFrame (frameChildren_ list))
                        WindowFrame _ _ ->
                          Frame id size tile children
                else
                  Frame id size tile (FrameFrame (frameChildren_ list))
              WindowFrame focus list ->
                f
    in
    -- remove empty windows first
    case windowFrame_ frame of
      Nothing -> frame
      Just f -> frame_ f -- merge single child frames

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
    split tab shadow f =
      -- XXX fix the IDs
      case shadow of
        Center ->
          case f of
            Frame id size tile c ->
              case c of
                WindowFrame focus l ->
                  Frame id size tile (WindowFrame focus (l ++ [tab]))
                _ -> f
        Top ->
          case f of
            Frame id size tile c ->
              case c of
                WindowFrame focus l ->
                  Frame id size tile (FrameFrame
                    [ Frame (id ++ "0") (Size size.width (size.height // 2)) Vert (WindowFrame 0 [tab])
                    , Frame (id ++ "1") (Size size.width (size.height - size.height // 2 - 1)) Vert (WindowFrame focus l)
                    ])
                _ -> f
        Right ->
          case f of
            Frame id size tile c ->
              case c of
                WindowFrame focus l ->
                  Frame id size tile (FrameFrame
                    [ Frame (id ++ "0") (Size (size.width // 2) size.height) Horiz (WindowFrame focus l)
                    , Frame (id ++ "1") (Size (size.width - size.width // 2 - 1) size.height) Horiz (WindowFrame 0 [tab])
                    ])
                _ -> f
        Bottom ->
          case f of
            Frame id size tile c ->
              case c of
                WindowFrame focus l ->
                  Frame id size tile (FrameFrame
                    [ Frame (id ++ "0") (Size size.width (size.height // 2)) Vert (WindowFrame focus l)
                    , Frame (id ++ "1") (Size size.width (size.height - size.height // 2 - 1)) Vert (WindowFrame 0 [tab])
                    ])
                _ -> f
        Left ->
          case f of
            Frame id size tile c ->
              case c of
                WindowFrame focus l ->
                  Frame id size tile (FrameFrame
                    [ Frame (id ++ "0") (Size (size.width // 2) size.height) Horiz (WindowFrame 0 [tab])
                    , Frame (id ++ "1") (Size (size.width - size.width // 2 - 1) size.height) Horiz (WindowFrame focus l)
                    ])
                _ -> f
        NoShadow ->
          f
    visitFrame d w f =
      let (id2, shadow) = case w of WindowPos id _ _ shadow _ _ -> (id, shadow) in
      case f of
        Frame id size tile c ->
          case c of
            FrameFrame l ->
              Frame id size tile (FrameFrame (visitFrameChildren d w l))
            WindowFrame focus l ->
              let visited = visitTabChildren d w l in
              if id == id2 && visited /= l then f
              else if id == id2 then
                split d.window shadow f
              else
                Frame id size tile (WindowFrame focus visited)
  in
  case drag of
    Nothing -> frame
    Just d ->
      case List.filter (\k -> case k of WindowPos _ _ _ shadow _ _ -> shadow /= NoShadow) windowList of
        [] -> frame
        hd :: _ ->  snuffEmpty (refocusTabs (reId (visitFrame d hd frame)))
