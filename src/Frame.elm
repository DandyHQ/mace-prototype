module Frame exposing (resizeAll, resize, focus, layoutWindows)

import Types exposing (..)

{-| scale children giving any left over to the last frame -}
resizeChildren : (Frame -> Size) -> (Frame -> Size) -> Size -> Size -> Tile -> List Frame -> List Frame
resizeChildren hScale vScale newSize rem tile l =
  case l of
    [] -> []
    hd :: [] ->
      resizeFrame hScale vScale rem tile hd :: []
    hd :: tl ->
      case tile of
        Horiz ->
          resizeFrame hScale vScale (hScale hd) tile hd
          :: resizeChildren hScale vScale newSize (Size (newSize.width - (hScale hd).width - 1) newSize.height) tile tl
        Vert ->
           resizeFrame hScale vScale (vScale hd) tile hd
           :: resizeChildren hScale vScale newSize (Size newSize.width (newSize.height - (vScale hd).height - 1)) tile tl
        -- any remaining cases are the result of an invalid tree
        _ ->
          resizeChildren hScale vScale newSize rem tile tl

{-| apply the resize, and then apply it to children as well -}
resizeFrame : (Frame -> Size) -> (Frame -> Size) -> Size -> Tile -> Frame -> Frame
resizeFrame hScale vScale newSize tile f =
  case f of
    Frame s t c ->
      Frame (if tile == Vert then newSize.height else newSize.width) t
        (case c of
          FrameFrame l ->
            FrameFrame (resizeChildren hScale vScale newSize newSize t l)
          WindowFrame id l ->
            WindowFrame id l
        )

{-| calculate new frame size for a horizontal scale -}
hScale : Size -> Size -> Frame -> Size
hScale oldSize newSize f =
  case f of
    Frame s _ _ ->
      Size (round (toFloat newSize.width / toFloat oldSize.width * toFloat s)) newSize.height

{-| calculate new frame size for a vertical scale -}
vScale : Size -> Size -> Frame -> Size
vScale oldSize newSize f =
  case f of
    Frame s _ _ ->
      Size newSize.width (round (toFloat newSize.height / toFloat oldSize.height * toFloat s))

{-| builds an hScale and vScale and passes it down through all the frames -}
resizeAll : Size -> Size -> Frame -> Frame
resizeAll oldSize newSize f =
  resizeFrame (hScale oldSize newSize) (vScale oldSize newSize) newSize None f


{-| resizes two frames according to the drag on their border -}
resize : Maybe ResizeDrag -> Frame -> Frame
resize drag frame =
  let
    resize_ d t l =
      case l of
        [] -> []
        hd :: [] -> resizeFrame_ d hd :: []
        a :: b :: tl ->
          if a == d.frame then
            -- we have found the frame. now do the resize
            case t of
              Horiz ->
                case a of
                  Frame s t c ->
                    Frame (s + d.current.x - d.start.x) t c
                ::
                case b of
                  Frame s t c ->
                    Frame (s - (d.current.x - d.start.x)) t c
                :: tl
              Vert ->
                case a of
                  Frame s t c ->
                    Frame (s + d.current.y - d.start.y) t c
                ::
                case b of
                  Frame s t c ->
                    Frame (s - (d.current.y - d.start.y)) t c
                :: tl
              -- something's wrong
              _ ->
                a :: b :: tl
          else
            a :: resize_ d t (b :: tl)

    resizeFrame_ d f =
      case f of
        Frame s t c ->
          case c of
            FrameFrame l ->
              Frame s t (FrameFrame (resize_ d t l))
            WindowFrame _ _ ->
              f
  in
  case drag of
    Nothing -> frame
    Just d ->
      resizeFrame_ d frame


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


{- positions windows at absoute positions -}
positionFrameChildren : Position -> Size -> Size -> Tile -> List Frame -> List WindowPositioned
positionFrameChildren pos size rem tile l =
  case l of
    [] -> []
    hd :: [] ->
      positionFrame (Position (pos.x + size.width - rem.width) (pos.y + size.height - rem.height)) rem tile hd
    hd :: tl ->
      positionFrame (Position (pos.x + size.width - rem.width) (pos.y + size.height - rem.height)) rem tile hd
        ++ case tile of
          Horiz ->
            positionFrameChildren pos size (Size (size.width - (getSize size tile hd).width - 1) size.height) tile tl
          Vert ->
            positionFrameChildren pos size (Size size.width (size.height - (getSize size tile hd).height - 1)) tile tl
          -- any remaining cases are the result of an invalid tree
          _ ->
            positionFrameChildren pos size rem tile tl

{- positioned windows at absolute positions -}
positionFrame : Position -> Size -> Tile -> Frame -> List WindowPositioned
positionFrame pos size tile f =
  case f of
    Frame s t c ->
        (case c of
          FrameFrame l ->
            positionFrameChildren pos (getSize size tile f) (getSize size tile f) t l
          WindowFrame focused l ->
            WindowPos pos (getSize size tile f) focused l :: []
        )

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

{- calculates the absolute positions of each window -}
layoutWindows : Size -> Frame -> List WindowPositioned
layoutWindows size frame =
  positionFrame (Position 0 0) size None frame
