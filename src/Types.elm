module Types exposing (..)

-- Window Name Contents
type Tab = Tab String String

-- Frame Size TileStyle (List Children)
type Frame = Frame Int Tile FrameChildren

type FrameChildren = FrameFrame (List Frame) | WindowFrame Int (List Tab)

type Tile = Horiz | Vert | NoTile

-- WindowPos Position Size Shadow ID (List Children)
type WindowPositioned = WindowPos Position Size Shadow Int (List Tab)

type Shadow = Top | Right | Bottom | Left | NoShadow

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
