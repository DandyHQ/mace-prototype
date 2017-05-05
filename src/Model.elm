module Model exposing (Model, init)

import Dict exposing (Dict)
import Frame
import Msg exposing (Msg)
import Task
import Types exposing (..)
import Window

type alias Model =
  { frames : Frame
  , resizeDrag : Maybe ResizeDrag
  , moveDrag : Maybe MoveDrag
  }

init : ( Model, Cmd Msg )
init =
  ( Model Frame.initial Nothing Nothing, Task.perform Msg.WindowResize Window.size )

files : Dict String String
files =
  Dict.fromList
    [ ("/root/tutorial2.py", "cat")
    , ("/root/example2.py", "frog")
    , ("/root/mouse.c", "pigeon")
    , ("/root/music.c", "dog")
    ]
