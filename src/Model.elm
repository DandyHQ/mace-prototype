module Model exposing (Model, init)

import Msg exposing (Msg)
import Task
import Types exposing (..)
import Window

type alias Model =
  { window : Size
  , frames : Frame
  , resizeDrag : Maybe ResizeDrag
  , moveDrag : Maybe MoveDrag
  }

init : ( Model, Cmd Msg )
init =
  ( Model (Window.Size 600 600) initialLayout Nothing Nothing, Task.perform Msg.WindowResize Window.size )

initialLayout : Frame
initialLayout =
  Frame 600 Horiz ( FrameFrame
    [ Frame 299 None ( WindowFrame 0
        [ Tab 1 "cat"
        , Tab 2 "dog"
        ])
    , Frame 299 Vert ( FrameFrame
        [ Frame 299 None
            ( WindowFrame 0 [ Tab 3 "tiger" ] )
        , Frame 299 None ( WindowFrame 0
            [ Tab 4 "pidgin"
            , Tab 5 "frog"
            , Tab 6 "song"
            ])
        ])
    ])
