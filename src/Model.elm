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
    [ Frame 299 Horiz ( WindowFrame 0
        [ Tab "/root/tutorial2.py" "cat"
        , Tab "/root/example2.py" "dog"
        ])
    , Frame 299 Horiz ( FrameFrame
        [ Frame 299 Vert
            ( WindowFrame 0 [ Tab "/root/readme.md" "tiger" ] )
        , Frame 299 Vert ( WindowFrame 0
            [ Tab "/root/mouse.c" "pidgin"
            , Tab "/root/example2.py" "frog"
            , Tab "/root/music.c" "song"
            ])
        ])
    ])
