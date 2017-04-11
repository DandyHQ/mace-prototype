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
    [ Frame 299 None ( WindowFrame
        [ Tab 1 True True "cat"
        , Tab 2 False False "dog"
        ])
    , Frame 299 Vert ( FrameFrame
        [ Frame 299 None
            ( WindowFrame [ Tab 3 False True "tiger" ] )
        , Frame 299 None ( WindowFrame
            [ Tab 4 False False "pidgin"
            , Tab 5 False True "frog"
            , Tab 6 False False "song"
            ])
        ])
    ])
