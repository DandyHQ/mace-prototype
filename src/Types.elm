module Types exposing (..)

-- Window ID
type Window = Window Int

-- Frame Width Height Offset TileStyle (List Children)
type Frame = Frame Int Int Int Tile FrameChildren

type FrameChildren = FrameFrame (List Frame) | WindowFrame (List Window)

type Tile = Horiz | Vert | None
