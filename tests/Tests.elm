module Tests exposing (..)

import Test exposing (..)
import Types exposing (..)
import Expect
import FilePath
import Frame
import Fuzz exposing (list, int, tuple, string)
import String


all : Test
all =
    describe "mace"
        [ filePath
        , resizeFrame
        ]

beforeResize : Frame
beforeResize =
  Frame "0" (Size 600 600) Horiz ( FrameFrame
    [ Frame "00" (Size 300 600) Horiz ( WindowFrame 0
        [ Tab "000" "/root/tutorial2.py" "cat"
        , Tab "001" "/root/example2.py" "dog"
        ])
    , Frame "00" (Size 299 600) Horiz ( FrameFrame
        [ Frame "000" (Size 299 300) Vert
            ( WindowFrame 0 [ Tab "0000" "/root/readme.md" "tiger" ] )
        , Frame "001" (Size 299 299) Vert ( WindowFrame 0
            [ Tab "0010" "/root/mouse.c" "pidgin"
            , Tab "0011" "/root/example2.py" "frog"
            , Tab "0012" "/root/music.c" "song"
            ])
        ])
    ])

{-| frame layout after it has been resized to (Size 1539 912) -}
afterResize : Frame
afterResize =
  Frame "0" (Size 1539 912) Horiz ( FrameFrame
    [ Frame "00" (Size 770 912) Horiz ( WindowFrame 0
        [ Tab "000" "/root/tutorial2.py" "cat"
        , Tab "001" "/root/example2.py" "dog"
        ])
    , Frame "00" (Size 768 912) Horiz ( FrameFrame
        [ Frame "000" (Size 768 456) Vert
            ( WindowFrame 0 [ Tab "0000" "/root/readme.md" "tiger" ] )
        , Frame "001" (Size 768 455) Vert ( WindowFrame 0
            [ Tab "0010" "/root/mouse.c" "pidgin"
            , Tab "0011" "/root/example2.py" "frog"
            , Tab "0012" "/root/music.c" "song"
            ])
        ])
    ])

-- this should be replaced with fuzzing
resizeFrame : Test
resizeFrame =
  describe "Resize Frame"
    [ test "perform a resize" <|
        \() ->
          Expect.equal afterResize (Frame.resizeAll (Size 1539 912) beforeResize)
    ]

filePath : Test
filePath =
  describe "File and Path Name Manipulation"
    [ describe "Take file name"
        [ test "simple path" <|
            \() ->
                Expect.equal "filename" (FilePath.takeFileName "/root/filename")
        , test "no directory" <|
            \() ->
                Expect.equal "filename" (FilePath.takeFileName "filename")
        , test "empty" <|
            \() ->
                Expect.equal "" (FilePath.takeFileName "")
        , test "root" <|
            \() ->
                Expect.equal "" (FilePath.takeFileName "/")
        , test "file in root" <|
            \() ->
                Expect.equal "filename" (FilePath.takeFileName "/filename")
        ]
    , describe "Take directory"
        [ test "simple path" <|
            \() ->
                Expect.equal "/root" (FilePath.takeDirectory "/root/filename")
        , test "multiple slashes" <|
            \() ->
                Expect.equal "/usr/local" (FilePath.takeDirectory "/usr//local///file")
        , test "no directory" <|
            \() ->
                Expect.equal "" (FilePath.takeDirectory "filename")
        , test "empty" <|
            \() ->
                Expect.equal "" (FilePath.takeDirectory "")
        , test "root" <|
            \() ->
                Expect.equal "/" (FilePath.takeDirectory "/")
        , test "file in root" <|
            \() ->
                Expect.equal "/" (FilePath.takeDirectory "/filename")
        ]
    , describe "Take extension"
        [ test "no extension" <|
            \() ->
                Expect.equal "" (FilePath.takeExtension "/root/filename")
        , test "full stop" <|
            \() ->
                Expect.equal "" (FilePath.takeExtension "/root/filename.")
        , test "simple extension" <|
            \() ->
                Expect.equal "exe" (FilePath.takeExtension "/root/filename.exe")
        ,test "two part extension" <|
            \() ->
                Expect.equal "tar.gz" (FilePath.takeExtension "/root/filename.tar.gz")
        , test "empty" <|
            \() ->
                Expect.equal "" (FilePath.takeExtension "")
        , test "root" <|
            \() ->
                Expect.equal "" (FilePath.takeExtension "/")
        , test "weird pattern" <|
            \() ->
                Expect.equal "nobody.home" (FilePath.takeExtension "/./.nobody.home")
        ]
    , describe "Drop extension"
        [ test "no extension" <|
            \() ->
                Expect.equal "/root/filename" (FilePath.dropExtension "/root/filename")
        , test "full stop" <|
            \() ->
                Expect.equal "/root/filename." (FilePath.dropExtension "/root/filename.")
        , test "simple extension" <|
            \() ->
                Expect.equal "/root/filename" (FilePath.dropExtension "/root/filename.exe")
        ,test "two part extension" <|
            \() ->
                Expect.equal "/root/filename" (FilePath.dropExtension "/root/filename.tar.gz")
        , test "empty" <|
            \() ->
                Expect.equal "" (FilePath.dropExtension "")
        , test "root" <|
            \() ->
                Expect.equal "/" (FilePath.dropExtension "/")
        , test "weird pattern" <|
            \() ->
                Expect.equal "/./" (FilePath.dropExtension "/./.nobody.home")
        ]
    , describe "Take base name"
        [ test "simple path" <|
            \() ->
                Expect.equal "filename" (FilePath.takeBaseName "/root/filename")
        , test "no directory" <|
            \() ->
                Expect.equal "filename" (FilePath.takeBaseName "filename")
        , test "empty" <|
            \() ->
                Expect.equal "" (FilePath.takeBaseName "")
        , test "root" <|
            \() ->
                Expect.equal "" (FilePath.takeBaseName "/")
        , test "file in root" <|
            \() ->
                Expect.equal "filename" (FilePath.takeBaseName "/filename")
        , test "full stop" <|
            \() ->
                Expect.equal "filename." (FilePath.takeBaseName "/root/filename.")
        , test "simple extension" <|
            \() ->
                Expect.equal "filename" (FilePath.takeBaseName "/root/filename.exe")
        , test "two part extension" <|
            \() ->
                Expect.equal "filename" (FilePath.takeBaseName "/root/filename.tar.gz")
        , test "weird pattern" <|
            \() ->
                Expect.equal "" (FilePath.takeBaseName "/./.nobody.home")
        ]
    ]
