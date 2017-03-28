module Update exposing (update)

import Model exposing (Model)
import Msg exposing (Msg(..))
import Types exposing (..)
import Window

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    -- DragStart f xy ->
    --   ( { model | drag = Just (Drag f xy xy) }, Cmd.none )
    --
    -- DragAt xy ->
    --   ( { model | drag = Maybe.map (\{frame, current} -> Drag frame current xy) model.drag }, Cmd.none )
    --
    -- DragEnd _ ->
    --   ( { model | drag = Nothing }, Cmd.none )
    --
    WindowResize newSize ->
      ( { model | window = newSize, frames = resize model.window newSize model.frames }, Cmd.none )

    _ ->
      ( model,  Cmd.none )

resizeChildren : (Int -> Int) -> (Int -> Int) -> Size -> Size -> Tile -> List Frame -> List Frame
resizeChildren hScale vScale newSize rem tile l =
  case l of
    [] -> []
    hd :: [] ->
      resizeFrame hScale vScale rem tile hd :: []
    hd :: tl ->
      hd :: tl
      -- case tile of
      --   Horiz ->
      --     resizeFrame hScale vScale (hScale hd)
      --     frameChildren size (Size (size.width - (getSize size tile hd).width - 2) size.height) tile tl
      --   Vert ->
      --     frameChildren size (Size size.width (size.height - (getSize size tile hd).height - 2)) tile tl
      --   -- any remaining cases are the result of an invalid tree
      --   _ ->
      --     frameChildren size rem tile tl

resizeFrame : (Int -> Int) -> (Int -> Int) -> Size -> Tile -> Frame -> Frame
resizeFrame hScale vScale newSize tile f =
  case f of
    Frame s t c ->
      Frame (if tile == Vert then newSize.height else newSize.width) t
        (case c of
          FrameFrame l ->
            FrameFrame (resizeChildren hScale vScale newSize newSize tile l)
          WindowFrame l ->
            WindowFrame l
        )
-- XXX
-- resize function, builds hScale and vScale. Passes it down
-- applied to children, leaving rem
-- resizeFrame just takes a new size and applies it
hScale : Size -> Size -> Int -> Int
hScale oldSize newSize s =
  round (toFloat newSize.width / toFloat oldSize.width * toFloat s)

vScale : Size -> Size -> Int -> Int
vScale oldSize newSize s =
  round (toFloat newSize.height / toFloat oldSize.height * toFloat s)

resize : Size -> Size -> Frame -> Frame
resize oldSize newSize f =
  resizeFrame (hScale oldSize newSize) (vScale oldSize newSize) newSize None f


--
-- moveFrame : Maybe Drag -> Frame -> Frame
-- moveFrame d f =
--   let
--     moveFrame_ f =
--       case f of
--         Frame size offset t children ->
--           case children of
--             WindowFrame _ ->
--               Frame size offset t children
--             FrameFrame l ->
--               Frame size offset t (FrameFrame (moveChildren t l))
--
--     moveChildren tile l =
--       case l of
--         [] -> []
--         hd :: [] ->
--           hd :: []
--         a :: b :: tl ->
--           -- Refactor so d isn't maybe
--           case d of
--             Nothing ->
--               a :: b :: moveChildren tile tl
--             Just {frame, start, current} ->
--               if frame == a then
--                 case a of
--                   Frame {width, height} o t children ->
--                     Frame (Size (width + 100) height) o t children
--                 ::
--                 case b of
--                   Frame {width, height} o t children ->
--                     Frame (Size (width - 100) height) (o + 100) t children
--                 :: tl
--               else
--                 a :: b :: moveChildren tile tl
--   in
--   moveFrame_ f
--
--
-- childrenResize : Size -> Size -> Tile -> FrameChildren -> FrameChildren
-- childrenResize oldSize newSize t children =
--   let
--     wScale w =
--       round (toFloat newSize.width / toFloat oldSize.width * toFloat w)
--
--     hScale h =
--       round (toFloat newSize.height / toFloat oldSize.height * toFloat h)
--
--     -- XXX no idea why this version works
--     frameResize newSize offset f =
--       case f of
--         Frame oldSize _ t children ->
--           Frame newSize offset t (childrenResize oldSize newSize t children)
--
--     -- remainder, index, list
--     childrenResize_ r i l =
--       let
--         newOff = if i > 0 then 2 else 0
--       in
--       case l of
--         [] -> []
--         h :: [] ->
--           case t of
--             Horiz -> frameResize (Size (r.width - newOff) r.height) (newSize.width - r.width + newOff) h :: []
--             Vert -> frameResize (Size r.width (r.height - newOff)) (newSize.height - r.height + newOff) h :: []
--             None -> frameResize r 0 h :: []
--         h :: tl ->
--           case h of
--             Frame {width, height} offset _ _ ->
--               case t of
--                 Horiz ->
--                   frameResize (Size (wScale width - newOff) (hScale height)) (newSize.width - r.width + newOff) h
--                   ::
--                     childrenResize_ (Size (r.width - (wScale width) - newOff) r.height) (i + 1) tl
--                 Vert ->
--                   frameResize (Size (wScale width) (hScale height - newOff)) (newSize.height - r.height + newOff) h
--                   ::
--                     childrenResize_ (Size r.width (r.height - (hScale height) - newOff)) (i + 1) tl
--                 None ->
--                   frameResize (Size (wScale width) (hScale height)) 0 h
--                   ::
--                     childrenResize_ r (i + 1) tl
--
--   in
--   case children of
--     FrameFrame l ->
--       FrameFrame (childrenResize_ newSize 0 l)
--     WindowFrame l ->
--       WindowFrame l
--
--
-- frameResize : Size -> Frame -> Frame
-- frameResize newSize f =
--   case f of
--     Frame oldSize offset t children ->
--       Frame newSize offset t (childrenResize oldSize newSize t children)
