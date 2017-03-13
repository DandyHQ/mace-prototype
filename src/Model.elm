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
  Frame 100 Horiz ( FrameFrame
    [ Frame 30 None ( WindowFrame
        [ Window 1
        , Window 2
        ])
    , Frame 70 Vert ( FrameFrame
        [ Frame 20 None
            ( WindowFrame [ Window 3 ] )
        , Frame 80 None ( WindowFrame
            [ Window 4
            , Window 5
            , Window 6
            ])
        ])
    ])
