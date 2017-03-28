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

-- scale children giving any left over to the last frame
resizeChildren : (Frame -> Size) -> (Frame -> Size) -> Size -> Size -> Tile -> List Frame -> List Frame
resizeChildren hScale vScale newSize rem tile l =
  case l of
    [] -> []
    hd :: [] ->
      resizeFrame hScale vScale rem tile hd :: []
    hd :: tl ->
      -- hd :: tl
      case tile of
        Horiz ->
          resizeFrame hScale vScale (hScale hd) tile hd
          :: resizeChildren hScale vScale newSize (Size (newSize.width - (hScale hd).width - 2) newSize.height) tile tl
        Vert ->
           resizeFrame hScale vScale (vScale hd) tile hd
           :: resizeChildren hScale vScale newSize (Size newSize.width (newSize.height - (vScale hd).height - 2)) tile tl
        -- any remaining cases are the result of an invalid tree
        _ ->
          resizeChildren hScale vScale newSize rem tile tl

-- apply the resize, and then apply it to children as well
resizeFrame : (Frame -> Size) -> (Frame -> Size) -> Size -> Tile -> Frame -> Frame
resizeFrame hScale vScale newSize tile f =
  case f of
    Frame s t c ->
      Frame (if tile == Vert then newSize.height else newSize.width) t
        (case c of
          FrameFrame l ->
            FrameFrame (resizeChildren hScale vScale newSize newSize t l)
          WindowFrame l ->
            WindowFrame l
        )

-- calculate new frame size for a horizontal scale
hScale : Size -> Size -> Frame -> Size
hScale oldSize newSize f =
  case f of
    Frame s _ _ ->
      Size (round (toFloat newSize.width / toFloat oldSize.width * toFloat s)) newSize.height

-- calculate new frame size for a vertical scale
vScale : Size -> Size -> Frame -> Size
vScale oldSize newSize f =
  case f of
    Frame s _ _ ->
      Size newSize.width (round (toFloat newSize.height / toFloat oldSize.height * toFloat s))

-- builds an hScale and vScale and passes it down through all the frames
resize : Size -> Size -> Frame -> Frame
resize oldSize newSize f =
  resizeFrame (hScale oldSize newSize) (vScale oldSize newSize) newSize None f
