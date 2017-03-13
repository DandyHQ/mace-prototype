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
  Frame Horiz ( FrameFrame
    [ Frame None ( WindowFrame
        [ Window 1
        , Window 2
        ])
    , Frame Vert ( FrameFrame
        [ Frame None
            ( WindowFrame [ Window 3 ] )
        , Frame None ( WindowFrame
            [ Window 4
            , Window 5
            , Window 6
            ])
        ])
    ])
