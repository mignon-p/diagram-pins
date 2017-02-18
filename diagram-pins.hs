import Codec.Picture( PixelRGBA8( .. ), writePng )
import Control.Monad
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations
import Graphics.Text.TrueType( Font, loadFontFile )
import System.IO.Unsafe

wpiPins =
  [ Just 17
  , Just 18
  , Just 27
  , Just 22
  , Just 23
  , Just 24
  , Just 25
  , Just 4
  , Just 2
  , Just 3
  , Just 8
  , Just 7
  , Just 10
  , Just 9
  , Just 11
  , Just 14
  , Just 15
  , Just 28
  , Just 29
  , Just 30
  , Just 31
  , Just 5
  , Just 6
  , Just 13
  , Just 19
  , Just 26
  , Just 12
  , Just 16
  , Just 20
  , Just 21
  , Just 0
  , Just 1
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  ]

physPins =
  [ Nothing
  , Nothing
  , Nothing
  , Just 2
  , Nothing
  , Just 3
  , Nothing
  , Just 4
  , Just 14
  , Nothing
  , Just 15
  , Just 17
  , Just 18
  , Just 27
  , Nothing
  , Just 22
  , Just 23
  , Nothing
  , Just 24
  , Just 10
  , Nothing
  , Just 9
  , Just 25
  , Just 11
  , Just 8
  , Nothing
  , Just 7
  , Just 0
  , Just 1
  , Just 5
  , Nothing
  , Just 6
  , Just 12
  , Just 13
  , Nothing
  , Just 19
  , Just 16
  , Just 26
  , Just 20
  , Nothing
  , Just 21
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Just 28
  , Just 29
  , Just 30
  , Just 31
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Nothing
  , Just 0
  ]

myFont :: Font
myFont = unsafePerformIO $ do
  (Right f) <- loadFontFile "Vera.ttf"
  return f

type Drwng = Drawing PixelRGBA8 ()

color r g b a = uniformTexture $ PixelRGBA8 r g b a

black = color 0 0 0 0xff
gray = color 0x80 0x80 0x80 0xff

red' = PixelRGBA8 0xff 0 0 0xff
green' = PixelRGBA8 0 0xff 0 0xff
blue' = PixelRGBA8 0 0 0xff 0xff

drawPin :: [(String, PixelRGBA8)] -> Drwng
drawPin pairs = withTexture black $ do
  fill $ circle (V2 0 0) 5
  stroke 3 JoinRound (CapRound, CapRound) $ line (V2 0 0) (V2 200 0)
  forM_ (zip pairs [0..]) $ \((str, c), i) -> do
    withTransformation (translate $ V2 (50 + i * 50) 0) $ do
      withTexture (uniformTexture c) $ fill $
        roundedRectangle (V2 0 (-10)) 40 20 10 10
      withTexture black $ printTextAt myFont (PointSize 12) (V2 5 5) str

drawing :: Drwng
drawing = do
  withTexture gray $ fill $ rectangle (V2 (-20) (-10)) 40 (20 + 20 * 32)
  withTransformation (translate $ V2 10 10) $
    drawPin [("Foo", red'), ("Bar", green'), ("Baz", blue')]

main :: IO ()
main = do
  let white = PixelRGBA8 255 255 255 255
      size = 800
      img = renderDrawing size size white $
            withTransformation (translate $ V2 400 20) $ drawing
  writePng "pin-diagram.png" img
