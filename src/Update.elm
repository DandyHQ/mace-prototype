module Update exposing (update)

import Model exposing (Model)
import Msg exposing (Msg(..))
import Types exposing (..)
import Window

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    WindowResize newSize ->
      ( { model | window = newSize, frames = frameResize newSize model.frames }, Cmd.none )
      
    _ ->
      ( model, Cmd.none )

childrenResize : Size -> Size -> Tile -> FrameChildren -> FrameChildren
childrenResize oldSize newSize t children =
  let
    wScale w =
      round (toFloat newSize.width / toFloat oldSize.width * toFloat w)

    hScale h =
      round (toFloat newSize.height / toFloat oldSize.height * toFloat h)

    -- XXX no idea why this version works
    frameResize newSize offset f =
      case f of
        Frame oldSize _ t children ->
          Frame newSize offset t (childrenResize oldSize newSize t children)

    -- remainder, index, list
    childrenResize_ r i l =
      let
        newOff = if i > 0 then 2 else 0
      in
      case l of
        [] -> []
        h :: [] ->
          case t of
            Horiz -> frameResize (Size (r.width - newOff) r.height) (newSize.width - r.width + newOff) h :: []
            Vert -> frameResize (Size r.width (r.height - newOff)) (newSize.height - r.height + newOff) h :: []
            None -> frameResize r 0 h :: []
        h :: tl ->
          case h of
            Frame {width, height} offset _ _ ->
              case t of
                Horiz ->
                  frameResize (Size (wScale width - newOff) (hScale height)) (newSize.width - r.width + newOff) h
                  ::
                    childrenResize_ (Size (r.width - (wScale width) - newOff) r.height) (i + 1) tl
                Vert ->
                  frameResize (Size (wScale width) (hScale height - newOff)) (newSize.height - r.height + newOff) h
                  ::
                    childrenResize_ (Size r.width (r.height - (hScale height) - newOff)) (i + 1) tl
                None ->
                  frameResize (Size (wScale width) (hScale height)) 0 h
                  ::
                    childrenResize_ r (i + 1) tl

  in
  case children of
    FrameFrame l ->
      FrameFrame (childrenResize_ newSize 0 l)
    WindowFrame l ->
      WindowFrame l


frameResize : Size -> Frame -> Frame
frameResize newSize f =
  case f of
    Frame oldSize offset t children ->
      Frame newSize offset t (childrenResize oldSize newSize t children)
