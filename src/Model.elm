module Model exposing (Model, init)

import Msg exposing (Msg)
import Task
import Types exposing (..)
import Window

type alias Model =
  { nextIndex : Int
  , window : Size
  , frames : Frame
  , drag : Maybe Drag
  }

type alias Drag =
  { start : Position
  , current : Position
  }

init : ( Model, Cmd Msg )
init =
  ( Model 2 (Window.Size 600 600) initialLayout Nothing, Task.perform Msg.WindowResize Window.size )

initialLayout : Frame
initialLayout =
  Frame (Size 600 600) 0 Horiz ( FrameFrame
    [ Frame (Size 299 600) 0 None ( WindowFrame
        [ Window 1
        , Window 2
        ])
    , Frame (Size 299 600) 301 Vert ( FrameFrame
        [ Frame (Size 299 299) 0 None
            ( WindowFrame [ Window 3 ] )
        , Frame (Size 299 299) 301 None ( WindowFrame
            [ Window 4
            , Window 5
            , Window 6
            ])
        ])
    ])
