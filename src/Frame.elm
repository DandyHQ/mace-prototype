module Frame exposing (initial, resizeAll, focus, resize, hover, rearrange)

import Types exposing (..)
import List.Extra as List
import Debug

borderWidth : Int
borderWidth = 1

mimimumSize : Size
mimimumSize = Size 20 20

{-| initial test layout of the application window -}
initial : Frame
initial =
  Frame "0" (Size 600 600) (Position 0 0) Horiz ( FrameFrame
    [ Frame "00" (Size 300 600) (Position 0 0) Horiz ( WindowFrame (Window NoShadow Nothing 0
        [ Tab "000" "/root/tutorial2.py" "cat"
        , Tab "001" "/root/example2.py" "dog"
        ]))
    , Frame "01" (Size 299 600) (Position 300 0) Horiz ( FrameFrame
        [ Frame "010" (Size 299 300) (Position 300 0) Vert
            ( WindowFrame (Window NoShadow Nothing 0 [ Tab "0000" "/root/readme.md" "tiger" ] ))
        , Frame "011" (Size 299 299) (Position 300 300) Vert ( WindowFrame (Window NoShadow Nothing 0
            [ Tab "0110" "/root/mouse.c" "pidgin"
            , Tab "0111" "/root/example2.py" "frog"
            , Tab "0112" "/root/music.c" "song"
            ]))
        ])
    ])

{-| resizes the frame tree to fit into the specified size -}
resizeAll : Size -> Frame -> Frame
resizeAll newSize frame =
  let
    scale a b c =
      round (toFloat a / toFloat b * toFloat c)
    scaleWidth = scale newSize.width frame.size.width
    scaleHeight = scale newSize.height frame.size.height
    children_ parentSize rem l =
      case l of
        [] -> []
        hd :: [] ->
          frame_ rem hd :: []
        hd :: tl ->
          let
            size =
              case hd.tile of
                Horiz -> Size (scaleWidth hd.size.width) parentSize.height
                Vert -> Size parentSize.width (scaleHeight hd.size.height)
                _ -> Size 0 0
            newRem =
              case hd.tile of
                Horiz -> Size (rem.width - size.width - borderWidth) parentSize.height
                Vert -> Size parentSize.width (rem.height - size.height - borderWidth)
                _ -> Size 0 0
          in
          frame_ size hd :: children_ parentSize newRem tl
    frame_ parentSize f =
      case f.children of
        FrameFrame list ->
          { f | size = parentSize, children = FrameFrame (children_ parentSize parentSize list) }
        WindowFrame _ ->
          { f | size = parentSize }

  in
  position (frame_ newSize frame)

{-| positions frames according to their sizes -}
position : Frame -> Frame
position frame =
  let
    children_ shift l =
      case l of
        [] -> []
        hd :: [] -> frame_ shift hd :: []
        hd :: tl ->
          frame_ shift hd
          ::
          case hd.tile of
            Horiz -> children_ (Position (hd.size.width + shift.x + borderWidth) shift.y) tl
            Vert -> children_ (Position shift.x (hd.size.height + shift.y + borderWidth)) tl
            _ -> children_ (Position 0 0) tl
    frame_ shift f =
      case f.children of
        FrameFrame list ->
          { f | pos = shift, children = FrameFrame (children_ shift list) }
        WindowFrame _ ->
          { f | pos = shift }
  in
  frame_ (Position 0 0) frame

{-| focuses a window from the current frame group -}
focus : Tab -> Frame -> Frame
focus tab frame =
  let
    children_ l =
      List.map frame_ l
    frame_ f =
      case f.children of
        FrameFrame list ->
          { f | children = FrameFrame (children_ list) }
        WindowFrame w ->
          case List.elemIndex tab w.tabs of
            Nothing -> f
            Just i ->
              { f | children = WindowFrame { w | focused = i } }
  in
  frame_ frame

{-| resizes two frames according to the drag on their border -}
resize : Maybe ResizeDrag -> Frame -> Frame
resize drag frame =
  let
    children_ d l =
      case l of
        [] -> []
        hd :: [] -> frame_ d hd :: []
        a :: b :: tl ->
          if a == d.frame then
            -- we have found the frame. now do the resize
            case a.tile of
              Horiz ->
                resizeAll (Size (a.size.width + (d.current.x - d.start.x)) a.size.height) a
                :: resizeAll (Size (b.size.width - (d.current.x - d.start.x)) b.size.height) b
                :: tl
              Vert ->
                resizeAll (Size a.size.width (a.size.height + (d.current.y - d.start.y))) a
                :: resizeAll (Size b.size.width (b.size.height - (d.current.y - d.start.y))) b
                :: tl
              -- something's wrong
              _ ->
                a :: b :: tl
          else
            frame_ d a :: children_ d (b :: tl)

    frame_ d f =
      case f.children of
        FrameFrame list ->
          { f | children = FrameFrame (children_ d list) }
        WindowFrame _ ->
          f
  in
  case drag of
    Nothing -> frame
    Just d ->
      position (frame_ d frame)

{-| gives a new identifier to each frame and tab -}
id : Frame -> Frame
id frame =
  let
    children_ id l =
      List.indexedMap (\k v -> frame_ (id ++ toString k) v) l
    tab_ id l =
      List.indexedMap (\k v -> { v | id = id ++ toString k }) l
    frame_ id f =
      case f.children of
        FrameFrame list ->
          { f | id = id, children = FrameFrame (children_ id list) }
        WindowFrame w ->
          { f | id = id, children = WindowFrame { w | tabs = tab_ id w.tabs } }
  in
  frame_ "0" frame

{-| takes a moveDrag and figures out where it is hovering -}
hover : Maybe MoveDrag -> Frame -> Frame
hover drag frame =
  case drag of
    Nothing -> frame
    Just d ->
      if not d.moved then
        frame
      else
        frame
          |> hoverTabBar d
          |> hoverFrame d

{-| takes a moveDrag and figures out if it's over a tab bar -}
hoverTabBar : MoveDrag -> Frame -> Frame
hoverTabBar drag frame =
  case frame.children of
    FrameFrame list ->
      { frame | children = FrameFrame (List.map (hoverTabBar drag) list) }
    WindowFrame w ->
      let
        tabSize =
          let average = frame.size.width // List.length w.tabs in
          if average > 200 then
            Size 200 30
          else
            Size average 30
        dragPos = Position (drag.current.x + drag.offset.x) (drag.current.y + drag.offset.y)
        hovering = dragPos.y > frame.pos.y - 15 && dragPos.y < frame.pos.y + 20 -- tab height is 30, tabBar 35
          && dragPos.x > frame.pos.x && dragPos.x < frame.pos.x + frame.size.width
      in
      if hovering then
        { frame | children = WindowFrame { w | hover = Just ((dragPos.x - frame.pos.x + 100) // tabSize.width) } }
      else
        frame

{-| takes a moveDrag and figures out if it's over a frame, causing a split -}
hoverFrame : MoveDrag -> Frame -> Frame
hoverFrame drag frame =
    case frame.children of
      FrameFrame list ->
        { frame | children = FrameFrame (List.map (hoverFrame drag) list) }
      WindowFrame w ->
        let
          dragPos = Position (drag.current.x + drag.offset.x + 100) (drag.current.y + drag.offset.y + 15)
          windowSize = Size frame.size.width (frame.size.height - 35)
          windowPos = Position frame.pos.x (frame.pos.y + 35)
          -- is there a cleaner way to do this?
          hovering =
            if dragPos.x > windowPos.x + windowSize.width // 4
              && dragPos.x < windowPos.x + windowSize.width - windowSize.width // 4
              && dragPos.y > windowPos.y + windowSize.height // 4
              && dragPos.y < windowPos.y + windowSize.height - windowSize.height // 4
            then
              Center
            else if dragPos.x > windowPos.x + windowSize.width // 4
              && dragPos.x < windowPos.x + windowSize.width - windowSize.width // 4
              && dragPos.y > windowPos.y
              && dragPos.y < windowPos.y + windowSize.height // 2
            then
              Top
            else if dragPos.x > windowPos.x + windowSize.width // 4
              && dragPos.x < windowPos.x + windowSize.width - windowSize.width // 4
              && dragPos.y > windowPos.y + windowSize.height // 2
              && dragPos.y < windowPos.y + windowSize.height
            then
              Bottom
            else if dragPos.x > windowPos.x
              && dragPos.x < windowPos.x + windowSize.width // 2
              && dragPos.y > windowPos.y
              && dragPos.y < windowPos.y + windowSize.height
            then
              Left
            else if dragPos.x > windowPos.x + (windowSize.width - windowSize.width // 2)
              && dragPos.x < windowPos.x + windowSize.width
              && dragPos.y > windowPos.y
              && dragPos.y < windowPos.y + windowSize.height
            then
              Right
            else
              NoShadow
        in
        { frame | children = WindowFrame { w | shadow = hovering } }


{-| takes a moveDrag and applies what effect the hover is specifying -}
rearrange : Maybe MoveDrag -> Frame -> Frame
rearrange drag frame =
  case drag of
    Nothing -> frame
    Just d ->
      if not d.moved then
        frame
      else
        let
          framed = applyHoverFrame d frame
          tabbed = applyHoverTabBar d frame
        in
        if framed /= frame then
          framed
        else if tabbed /= frame then
          tabbed
        else
          frame

{-| calls hoverTabBar and applies the transformation it represents -}
applyHoverTabBar : MoveDrag -> Frame -> Frame
applyHoverTabBar drag frame =
  let
    hovered = hoverTabBar drag frame
    frame_ f =
      case f.children of
        FrameFrame list ->
          { f | children = FrameFrame (List.map frame_ list) }
        WindowFrame w ->
          case w.hover of
            Nothing ->
              let
                updatedTabs = List.remove drag.tab w.tabs
              in
              { f | children = WindowFrame
                { w | hover = Nothing
                , tabs = updatedTabs
                , focused = min w.focused (List.length updatedTabs - 1)
                }
              }
            Just index ->
              let
                dragTab = drag.tab -- name.with.dot doesn't work in record update syntax
                newTab = { dragTab | id = "new" } -- stops the possible concurrency bug of inserting and removing
                updatedTabs = List.remove drag.tab (insertAt index newTab w.tabs)
              in
              { f | children = WindowFrame
                { w | hover = Nothing
                , tabs = updatedTabs
                , focused = (case List.elemIndex newTab updatedTabs of
                      Nothing -> 0
                      Just index -> index
                    )
                }
              }
  in
  if hovered == frame then
    frame
  else
    prune (id (frame_ hovered))

{-| calls hoverFrame and applies the transformation it represents -}
applyHoverFrame : MoveDrag -> Frame -> Frame
applyHoverFrame drag frame =
  let
    hovered = hoverFrame drag frame
    frame_ f =
      case f.children of
        FrameFrame list ->
          { f | children = FrameFrame (List.map frame_ list) }
        WindowFrame w ->
          let
            updatedTabs = List.remove drag.tab w.tabs
          in
          case w.shadow of
            NoShadow ->
              { f | children = WindowFrame { w | tabs = updatedTabs } }
            Center ->
              if updatedTabs /= w.tabs then
                -- we are adding back the tab we just removed
                { f | children = WindowFrame { w | shadow = NoShadow } }
              else
                { f | children = WindowFrame { w | shadow = NoShadow, tabs = w.tabs ++ [ drag.tab ] } }
            Top ->
              { f | children = FrameFrame (
                Frame "new" (Size f.size.width (f.size.height // 2)) (Position 0 0) Vert
                  (WindowFrame (Window NoShadow Nothing 0 [drag.tab]))
                ::
                Frame "new" (Size f.size.width (f.size.height - f.size.height // 2 - 1)) (Position 0 0) Vert
                  (WindowFrame { w | shadow = NoShadow, tabs = updatedTabs })
                :: []
              )}
            Right ->
              { f | children = FrameFrame (
                Frame "new" (Size (f.size.width // 2) f.size.height) (Position 0 0) Horiz
                  (WindowFrame { w | shadow = NoShadow, tabs = updatedTabs })
                ::
                Frame "new" (Size (f.size.width - f.size.width // 2 - 1) f.size.height) (Position 0 0) Horiz
                  (WindowFrame (Window NoShadow Nothing 0 [drag.tab]))
                :: []
              )}
            Bottom ->
              { f | children = FrameFrame (
                Frame "new" (Size f.size.width (f.size.height // 2)) (Position 0 0) Vert
                  (WindowFrame { w | shadow = NoShadow, tabs = updatedTabs })
                ::
                Frame "new" (Size f.size.width (f.size.height - f.size.height // 2 - 1)) (Position 0 0) Vert
                  (WindowFrame (Window NoShadow Nothing 0 [drag.tab]))
                :: []
              )}
            Left ->
              { f | children = FrameFrame (
                Frame "new" (Size (f.size.width // 2) f.size.height) (Position 0 0) Horiz
                  (WindowFrame (Window NoShadow Nothing 0 [drag.tab]))
                ::
                Frame "new" (Size (f.size.width - f.size.width // 2 - 1) f.size.height) (Position 0 0) Horiz
                  (WindowFrame { w | shadow = NoShadow, tabs = updatedTabs })
                :: []
              )}
  in
  if hovered == frame then
    frame
  else
    prune (id (frame_ hovered))


{-| removes empty nodes from the tree -}
prune : Frame -> Frame
prune frame =
  let
    notEmpty f = case f.children of
      FrameFrame list ->
        List.length list > 0
      WindowFrame w ->
        List.length w.tabs > 0
    frame_ f =
      case f.children of
        FrameFrame list ->
          let filtered = List.filter notEmpty list in
          case filtered of
            [] -> { f | children = FrameFrame [] }
            hd :: [] -> resizeAll f.size { hd | tile = f.tile }
            hd :: tl -> { f | children = FrameFrame (List.map frame_ filtered) }
        WindowFrame w ->
          f
  in
  frame_ frame |> id |> position


{-| helper function. inserts a value at index into a list, or at the end of index too large -}
insertAt : Int -> a -> List a -> List a
insertAt key value list =
  case list of
    [] -> value :: []
    hd :: tl ->
      if key == 0 then
        value :: hd :: tl
      else
        hd :: insertAt (key - 1) value tl

-- hoverWindow : MoveDrag -> Frame -> Frame
-- {-| figures out whether a window should be divided if the hovering tab is dropped -}
-- tabShadow : Maybe MoveDrag -> List WindowPositioned -> List WindowPositioned
-- tabShadow drag windowList =
--   let
--     windowShadowed d w =
--       case w of
--         WindowPos id pos size _ focused l ->
--           -- top
--           if (d.current.y + d.offset.y) > pos.y + 35 && (d.current.y + d.offset.y) < pos.y + (size.height - 35) // 2
--             && (d.current.x + d.offset.x) > pos.x + 100 && (d.current.x + d.offset.x) < pos.x + size.width - 300
--             then
--             WindowPos id pos size Top focused l
--           -- right
--           else if (d.current.y + d.offset.y) > pos.y + 35 && (d.current.y + d.offset.y) < pos.y + size.height
--             && (d.current.x + d.offset.x) > pos.x + size.width - 300 && (d.current.x + d.offset.x) < pos.x + size.width - 100
--             then
--             WindowPos id pos size Right focused l
--           -- bottom
--           else if (d.current.y + d.offset.y) > pos.y + 35 + size.height // 2 && (d.current.y + d.offset.y) < pos.y + size.height
--             && (d.current.x + d.offset.x) > pos.x + 100 && (d.current.x + d.offset.x) < pos.x + size.width - 300
--             then
--             WindowPos id pos size Bottom focused l
--           -- left
--           else if (d.current.y + d.offset.y) > pos.y + 35 && (d.current.y + d.offset.y) < pos.y + size.height
--             && (d.current.x + d.offset.x) > pos.x - 100 && (d.current.x + d.offset.x) < pos.x + 100
--             then
--             WindowPos id pos size Left focused l
--           else if (d.current.y + d.offset.y) > pos.y + 35 && (d.current.y + d.offset.y) < pos.y + size.height
--             && (d.current.x + d.offset.x) > pos.x && (d.current.x + d.offset.x) < pos.x + size.width - 100
--             then
--             WindowPos id pos size Center focused l
--           else
--             w
--   in
--   case drag of
--     Nothing -> windowList
--     Just d -> List.map (windowShadowed d) windowList
--
--
--
-- {-| ensures no windows are focusing on non-existant tabs -}
-- refocusTabs : Frame -> Frame
-- refocusTabs frame =
--     let
--       frameChildren_ l =
--         case l of
--           [] -> []
--           hd :: tl ->
--             frame_ hd :: frameChildren_ tl
--       frame_ f =
--         case f of
--           Frame id size pos tile children ->
--             case children of
--               FrameFrame list ->
--                 Frame id size pos tile (FrameFrame (frameChildren_ list))
--               WindowFrame shadow hover focus list ->
--                 Frame id size pos tile (WindowFrame shadow hover
--                   (if focus < List.length list - 1 then
--                     focus
--                   else
--                     List.length list - 1)
--                   list)
--     in
--     frame_ frame
--
-- {-| removes any empty frames merging them with neighbours -}
-- snuffEmpty : Frame -> Frame
-- snuffEmpty frame =
--     let
--       -- remove any empty windows
--       windowFrameChildren_ l =
--         case l of
--           [] -> []
--           hd :: tl ->
--             case windowFrame_ hd of
--               Nothing -> windowFrameChildren_ tl
--               Just f -> f :: windowFrameChildren_ tl
--       windowFrame_ f =
--         case f of
--           Frame id size pos tile children ->
--             case children of
--               FrameFrame list ->
--                 Just (Frame id size pos tile (FrameFrame (windowFrameChildren_ list)))
--               WindowFrame shadow hover focus list ->
--                 if List.length list == 0 then
--                   Nothing
--                 else
--                   Just (Frame id size pos tile (WindowFrame shadow hover focus list))
--       frameChildren_ l =
--         case l of
--           [] -> []
--           hd :: tl -> frame_ hd :: frameChildren_ tl
--       frame_ f =
--         case f of
--           Frame id size pos tile children ->
--             case children of
--               FrameFrame list ->
--                 if List.length list == 1 then
--                   case Maybe.withDefault f (List.head list) of
--                     Frame _ _ _ children ->
--                       case children of
--                         FrameFrame list ->
--                           Frame id size pos tile (FrameFrame (frameChildren_ list))
--                         WindowFrame _ _ ->
--                           Frame id size pos tile children
--                 else
--                   Frame id size pos tile (FrameFrame (frameChildren_ list))
--               WindowFrame shadow hover focus list ->
--                 f
--     in
--     -- remove empty windows first
--     case windowFrame_ frame of
--       Nothing -> frame
--       Just f -> frame_ f -- merge single child frames
--
-- {-| find the dragged tab and moves it into the frame that is being hovered over -}
-- applySplit : Maybe MoveDrag -> List WindowPositioned -> Frame -> Frame
-- applySplit drag windowList frame =
--   let
--     visitFrameChildren d w l =
--       case l of
--         [] -> []
--         hd :: tl ->
--           visitFrame d w hd :: visitFrameChildren d w tl
--     visitTabChildren d w l =
--       case l of
--         [] -> []
--         hd :: tl ->
--           -- remove the tab
--           if hd == d.window then
--             visitTabChildren d w tl
--           else
--             hd :: visitTabChildren d w tl
--     split tab shadow f =
--       -- XXX fix the IDs
--       case shadow of
--         Center ->
--           case f of
--             Frame id size pos tile c ->
--               case c of
--                 WindowFrame shadow hover focus l ->
--                   Frame id size pos tile (WindowFrame shadow hover focus (l ++ [tab]))
--                 _ -> f
--         Top ->
--           case f of
--             Frame id size pos tile c ->
--               case c of
--                 WindowFrame shadow hover focus l ->
--                   Frame id size tile (FrameFrame
--                     [ Frame (id ++ "0") (Size size.width (size.height // 2)) pos Vert (WindowFrame shadow hover 0 [tab])
--                     , Frame (id ++ "1") (Size size.width (size.height - size.height // 2 - 1)) pos Vert (WindowFrame shadow hover focus l)
--                     ])
--                 _ -> f
--         Right ->
--           case f of
--             Frame id size pos tile c ->
--               case c of
--                 WindowFrame shadow hover focus l ->
--                   Frame id size tile (FrameFrame
--                     [ Frame (id ++ "0") (Size (size.width // 2) size.height) pos Horiz (WindowFrame shadow hover focus l)
--                     , Frame (id ++ "1") (Size (size.width - size.width // 2 - 1) size.height) pos Horiz (WindowFrame shadow hover 0 [tab])
--                     ])
--                 _ -> f
--         Bottom ->
--           case f of
--             Frame id size pos tile c ->
--               case c of
--                 WindowFrame shadow hover focus l ->
--                   Frame id size tile (FrameFrame
--                     [ Frame (id ++ "0") (Size size.width (size.height // 2)) pos Vert (WindowFrame shadow hover focus l)
--                     , Frame (id ++ "1") (Size size.width (size.height - size.height // 2 - 1)) pos Vert (WindowFrame shadow hover 0 [tab])
--                     ])
--                 _ -> f
--         Left ->
--           case f of
--             Frame id size pos tile c ->
--               case c of
--                 WindowFrame shadow hover focus l ->
--                   Frame id size tile (FrameFrame
--                     [ Frame (id ++ "0") (Size (size.width // 2) size.height) pos Horiz (WindowFrame shadow hover 0 [tab])
--                     , Frame (id ++ "1") (Size (size.width - size.width // 2 - 1) size.height) pos Horiz (WindowFrame shadow hover focus l)
--                     ])
--                 _ -> f
--         NoShadow ->
--           f
--     visitFrame d w f =
--       let (id2, shadow) = case w of WindowPos id _ _ shadow _ _ -> (id, shadow) in
--       case f of
--         Frame id size tile c ->
--           case c of
--             FrameFrame l ->
--               Frame id size tile (FrameFrame (visitFrameChildren d w l))
--             WindowFrame focus l ->
--               let visited = visitTabChildren d w l in
--               if id == id2 && visited /= l then f
--               else if id == id2 then
--                 split d.window shadow f
--               else
--                 Frame id size tile (WindowFrame focus visited)
--   in
--   case drag of
--     Nothing -> frame
--     Just d ->
--       case List.filter (\k -> case k of WindowPos _ _ _ shadow _ _ -> shadow /= NoShadow) windowList of
--         [] -> frame
--         hd :: _ ->  snuffEmpty (refocusTabs (reId (visitFrame d hd frame)))
