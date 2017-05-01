module FrameTests exposing(all)

import Test exposing (..)
import Types exposing (..)
import Expect
import Frame
import Fuzz exposing (list, int, tuple, string)

all : Test
all =
    describe "frame tests"
        [ --resizeFrame
        ]

checkResize : Size -> Frame -> Bool
checkResize size frame =
  let
    mimimumSize =
      let
        children_ l =
          case l of
            [] -> 0
            hd :: tl -> 0
        frame_ f = 4
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
