module Types exposing (..)

-- Window ID
type Window = Window Int

-- Frame Size TileStyle (List Children)
type Frame = Frame Int Tile FrameChildren

type FrameChildren = FrameFrame (List Frame) | WindowFrame (List Window)

type Tile = Horiz | Vert | None

type alias Size =
  { width : Int
  , height : Int
  }

type alias Position =
  { x : Int
  , y : Int
  }

type alias Drag =
  { frame : Frame
  , start : Position
  , current : Position
  }
