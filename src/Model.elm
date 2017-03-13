module Model exposing (Model, init)

import Msg exposing (Msg)
import Types exposing (Window(..), Frame(..), FrameChildren(..), Tile(..))

type alias Model =
  { nextIndex : Int
  , frames : Frame
  }

init : ( Model, Cmd Msg )
init =
  ( Model 2 initialLayout, Cmd.none )

initialLayout : Frame
initialLayout =
  Frame 1 100 Horiz ( FrameFrame
    [ Frame 2 30 None ( WindowFrame
        [ Window 3
        , Window 4
        ])
    , Frame 5 70 Vert ( FrameFrame
        [ Frame 6 20 None
            ( WindowFrame [ Window 7 ] )
        , Frame 8 80 None ( WindowFrame
            [ Window 9
            , Window 10
            , Window 11
            ])
        ])
    ])
