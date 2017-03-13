module Model exposing (Model, init)

import Msg exposing (Msg)
import Task
import Types exposing (Window(..), Frame(..), FrameChildren(..), Tile(..))
import Window

type alias Model =
  { nextIndex : Int
  , window : Window.Size
  , frames : Frame
  }

init : ( Model, Cmd Msg )
init =
  ( Model 2 (Window.Size 0 0) initialLayout, Task.perform Msg.WindowResize Window.size )

initialLayout : Frame
initialLayout =
  Frame 600 600 Horiz ( FrameFrame
    [ Frame 296 596 None ( WindowFrame
        [ Window 1
        , Window 2
        ])
    , Frame 296 596 Vert ( FrameFrame
        [ Frame 296 292 None
            ( WindowFrame [ Window 3 ] )
        , Frame 296 292 None ( WindowFrame
            [ Window 4
            , Window 5
            , Window 6
            ])
        ])
    ])
