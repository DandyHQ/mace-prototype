module Types exposing (..)

-- Window ID Name Contents
-- type Tab = Tab String String String

-- Frame ID Size Position TileStyle (List Children)
-- type Frame = Frame String Size Position Tile FrameChildren
--
-- type FrameChildren = FrameFrame (List Frame) | WindowFrame Shadow (Maybe Int) Int (List Tab)

type alias Tab =
  { id : String
  , path : String
  , contents : String
  }

type alias Frame =
  { id : String
  , size : Size
  , pos : Position
  , tile : Tile
  , children : FrameChildren
  }

type alias Window =
  { shadow : Shadow
  , hover : Maybe Int
  , focused : Int
  , tabs : List Tab
  }

type FrameChildren = FrameFrame (List Frame) | WindowFrame Window


type Tile = Horiz | Vert | NoTile

-- WindowPos ID Position Size Shadow FocusedIndex (List Children)
type WindowPositioned = WindowPos String Position Size Shadow Int (List Tab)

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
  { tab : Tab
  , moved : Bool
  , offset : Position
  , start : Position
  , current : Position
  }
