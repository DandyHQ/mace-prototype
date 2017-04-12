module Types exposing (..)

-- Window ID Contents
type Tab = Tab Int String

-- Frame Size TileStyle (List Children)
type Frame = Frame Int Tile FrameChildren

type FrameChildren = FrameFrame (List Frame) | WindowFrame Int (List Tab)

type WindowPositioned = WindowPos Position Size Int (List Tab)

type Tile = Horiz | Vert | None

type alias Size =
  { width : Int
  , height : Int
  }

type alias Position =
  { x : Int
  , y : Int
  }

type alias ResizeDrag =
  { frame : Frame
  , start : Position
  , current : Position
  }

type alias MoveDrag =
  { window : Tab
  , moved : Bool
  , offset : Position
  , start : Position
  , current : Position
  }
