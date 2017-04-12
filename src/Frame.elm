module Frame exposing (resizeAll, focus, layoutWindows)

import Types exposing (..)

{- applies a function to all frames -}
map : (Frame -> Frame) -> Frame -> Frame
map fn frame =
  let
    frame_ f =
      case f of
        Frame s t c ->
          fn (Frame s t (case c of
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
        Frame s t c ->
          case t of
            Horiz -> Frame (round (toFloat newSize.width / toFloat oldSize.width * toFloat s)) t c
            Vert -> Frame (round (toFloat newSize.height / toFloat oldSize.height * toFloat s))t c
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


{- focuses a window from the current frame group -}
focus : Tab -> Frame -> Frame
focus window frame =
  let
    frameChildren_ l =
      List.indexedMap frame_ l
    frame_ i f =
      case f of
        Frame s t c ->
          case c of
            FrameFrame l ->
              Frame s t (FrameFrame (frameChildren_ l))
            WindowFrame _ l ->
              let
                focused = List.sum (List.indexedMap (\k v -> if v == window then k + 1 else 0) l)
              in
              if focused /= 0 then
                Frame s t (WindowFrame (focused - 1) l)
              else
                f
  in
    frame_ 0 frame


{- mutually recursive with positionFrame to perform a window layout -}
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
            Frame s t _ -> (s, t)
        size =
          case tile of
            Horiz -> Size (parentSize.width - s - 1) parentSize.height
            Vert -> Size parentSize.width (parentSize.height - s - 1)
            None -> parentSize
        rem_ =
          case tile of
            Horiz -> Size (rem.width - size.width - 1) rem.height
            Vert -> Size rem.width (rem.height - size.height - 1)
            None -> rem
      in
      positionFrame pos_ size hd
      ++ positionFrameChildren pos parentSize rem_ tl

{- mutually recursive with positionFrameChildren to perform a window layout -}
positionFrame : Position -> Size -> Frame -> List WindowPositioned
positionFrame pos size f =
  case f of
    Frame s t c ->
      case c of
        FrameFrame l ->
          positionFrameChildren pos size size l
        WindowFrame focused l ->
          WindowPos pos size focused l :: []

{- lays the windows out, positioned absolutely within the size (assumes they have been resized first) -}
layoutWindows : Size -> Frame -> List WindowPositioned
layoutWindows size frame =
  positionFrame (Position 0 0) size frame
