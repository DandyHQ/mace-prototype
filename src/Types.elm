module Types exposing (..)

-- Window ID
type Window = Window Int

-- Frame Size Offset TileStyle (List Children)
type Frame = Frame Size Int Tile FrameChildren

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
