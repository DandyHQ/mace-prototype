module Model exposing (Model, init)

import Frame
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
  ( Model (Window.Size 600 600) Frame.initial Nothing Nothing, Task.perform Msg.WindowResize Window.size )
