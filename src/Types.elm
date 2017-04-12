module Types exposing (..)

-- Window ID Name Contents
type Tab = Tab Int String String

-- Frame ID Size TileStyle (List Children)
type Frame = Frame Int Int Tile FrameChildren

type FrameChildren = FrameFrame (List Frame) | WindowFrame Int (List Tab)

type Tile = Horiz | Vert | NoTile

-- WindowPos ID Position Size Shadow FocusedIndex (List Children)
type WindowPositioned = WindowPos Int Position Size Shadow Int (List Tab)

type Shadow = Center | Top | Right | Bottom | Left | NoShadow

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
