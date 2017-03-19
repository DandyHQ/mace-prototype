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
  Frame 600 600 0 Horiz ( FrameFrame
    [ Frame 299 600 0 None ( WindowFrame
        [ Window 1
        , Window 2
        ])
    , Frame 299 600 301 Vert ( FrameFrame
        [ Frame 299 299 0 None
            ( WindowFrame [ Window 3 ] )
        , Frame 299 299 301 None ( WindowFrame
            [ Window 4
            , Window 5
            , Window 6
            ])
        ])
    ])
