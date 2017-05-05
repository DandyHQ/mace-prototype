module FrameTests exposing(..)

import Debug
import Test exposing (..)
import Types exposing (..)
import Expect
import Frame exposing (borderWidth, minimumSize)
import Fuzz exposing (list, int, tuple, tuple5, string)

all : Test
all =
    describe "frame tests"
        [ resizeFrame
        ]

{-| Calculates the total minimum size of a frame -}
totalMinimum : Frame -> Size
totalMinimum frame =
  let
    children_ l s =
      case l of
        [] -> s
        hd :: [] ->
          case hd.tile of
            Horiz -> Size (s.width + (frame_ hd).width) (frame_ hd).height
            Vert -> Size (frame_ hd).width (s.height + (frame_ hd).height)
            NoTile -> minimumSize
        hd :: tl ->
          case hd.tile of
            Horiz ->
              children_ tl (Size (s.width + (frame_ hd).width + borderWidth) (frame_ hd).height)
            Vert ->
              children_ tl (Size (frame_ hd).width (s.height + (frame_ hd).height + borderWidth))
            NoTile -> Size 0 0
    frame_ f =
      case f.children of
        FrameFrame list ->
          children_ list (Size 0 0)
        WindowFrame _ ->
          minimumSize
  in
  frame_ frame

{-| Checks that the size of the children added together equals the size of the parent -}
childrenAddToParent : Frame -> Bool
childrenAddToParent frame =
  case frame.children of
    FrameFrame list ->
      frame.size == Frame.sumChildren list
      && List.all childrenAddToParent list
    WindowFrame _ ->
      True

{-| Check that the resize is correct and larger than the minimum -}
checkResizeAll : Size -> Frame -> Bool
checkResizeAll size frame =
  let
    minSize = totalMinimum frame
    newSize =
      Size
        (if minSize.width > size.width then minSize.width else size.width)
        (if minSize.height > size.height then minSize.height else size.height)
    updatedFrame =
      Frame.resizeAll size frame
  in
  -- can be out by a pixel from rounding error when tasked with the minimum size
  -- but users won't notice so we don't car
  abs (updatedFrame.size.width - newSize.width) < 2
  && abs (updatedFrame.size.height - newSize.height) < 2
  && childrenAddToParent updatedFrame

{-| Turns all the frames into a list, and extracts an index -}
getFrameByIndex : Int -> Frame -> Frame
getFrameByIndex index frame =
  let
    frame_ f =
      case f.children of
        FrameFrame list ->
          f :: List.concatMap frame_ list
        WindowFrame _ ->
          [f]
    getByIndex i l =
      case l of
        [] -> frame -- appease the compiler
        hd :: [] -> hd
        hd :: tl -> if i == 0 then hd else getByIndex (i - 1) tl
  in
  getByIndex index (frame_ frame)

checkResize : ResizeDrag -> Frame -> Bool
checkResize drag frame =
  let
    resized = Frame.resize (Just drag) frame
  in
  childrenAddToParent resized
  && frame.size == resized.size

resizeFrame : Test
resizeFrame =
  describe "Resize Frame"
    [ fuzz (tuple (int, int)) "resizes all frames" <|
        \(a, b) ->
          Frame.initial
            |> Frame.resizeAll (Size a b)
            |> checkResizeAll (Size a b)
            |> Expect.true "Expected resizeAll to have valid properties"
    -- this test could be smarter
    , fuzz (tuple5 (int, int, int, int, int)) "resizes a partition between frames" <|
        \(id, x1, y1, x2, y2) ->
          let drag = ResizeDrag (getFrameByIndex id Frame.initial) (Position x1 y1) (Position x2 y2) in
          Frame.initial
            |> Frame.resize (Just drag)
            |> checkResize drag
            |> Expect.true "Expected resize to have valid properties"
    ]
