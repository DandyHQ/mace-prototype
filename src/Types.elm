module Types exposing (..)

-- Window ID Focused Visible Contents
type Tab = Tab Int Bool Bool String

-- Frame Size TileStyle (List Children)
type Frame = Frame Int Tile FrameChildren

type FrameChildren = FrameFrame (List Frame) | WindowFrame (List Tab)

type WindowPositioned = WindowPos Position Size (List Tab)

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
