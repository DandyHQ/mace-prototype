module FrameTests exposing(all)

import Debug
import Test exposing (..)
import Types exposing (..)
import Expect
import Frame exposing (borderWidth, minimumSize)
import Fuzz exposing (list, int, tuple, string)

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

{-| Check that the resize is correct and larger than the minimum -}
checkResize : Size -> Frame -> Bool
checkResize size frame =
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

resizeFrame : Test
resizeFrame =
  describe "Resize Frame"
    [ fuzz (tuple (int, int)) "resizes all frames" <|
        \(a, b) ->
          Frame.initial
            |> Frame.resizeAll (Size a b)
            |> checkResize (Size a b)
            |> Expect.true "Expected outer frame to equal new size or the minimum size"
    ]
