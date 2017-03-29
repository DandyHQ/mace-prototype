module Model exposing (Model, init)

import Msg exposing (Msg)
import Task
import Types exposing (..)
import Window

type alias Model =
  { window : Size
  , frames : Frame
  , drag : Maybe Drag
  }

init : ( Model, Cmd Msg )
init =
  ( Model (Window.Size 600 600) initialLayout Nothing, Task.perform Msg.WindowResize Window.size )

initialLayout : Frame
initialLayout =
  Frame 600 Horiz ( FrameFrame
    [ Frame 299 None ( WindowFrame
        [ Window 1
        , Window 2
        ])
    , Frame 299 Vert ( FrameFrame
        [ Frame 299 None
            ( WindowFrame [ Window 3 ] )
        , Frame 299 None ( WindowFrame
            [ Window 4
            , Window 5
            , Window 6
            ])
        ])
    ])
