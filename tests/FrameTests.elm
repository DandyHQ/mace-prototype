module FrameTests exposing(all)

import Test exposing (..)
import Types exposing (..)
import Expect
import Frame exposing (borderWidth, minimumSize)
import Fuzz exposing (list, int, tuple, string)

all : Test
all =
    describe "frame tests"
        [ --resizeFrame
        ]

checkResize : Size -> Frame -> Bool
checkResize size frame =
  let
    totalMinimum =
      let
        children_ l s =
          case l of
            [] -> s
            hd :: [] ->
              case hd.tile of
                Horiz -> Size (s.width + minimumSize.width) minimumSize.height
                Vert -> Size minimumSize.width (s.height + minimumSize.height)
                NoTile -> minimumSize
            hd :: tl ->
              case hd.tile of
                Horiz ->
                  children_ tl (Size (s.width + minimumSize.width + borderWidth) minimumSize.height)
                Vert ->
                  children_ tl (Size minimumSize.width (s.height + minimumSize.height + borderWidth))
                NoTile -> Size 0 0
        frame_ f =
          case f.children of
            FrameFrame list ->
              children_ list (Size 0 0)
            WindowFrame _ ->
              Size 0 0
      in
      frame_ frame
  in
  False


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
