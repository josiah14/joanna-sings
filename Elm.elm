import Maybe
import Text
import Window

main = banner <~ Window.dimensions

linkStyle : Int -> Int -> Text.Style
linkStyle w h = { typeface = ["century gothic", "sans-serif"]
                , height = Just <| percentOfDiag (w,h) 0.05
                , color = white
                , bold = False
                , italic = True
                , line = Nothing
                }

bannerBg : (Int, Int) -> Element
bannerBg (w, h) = opacity 0.5 <| color black <| container w (ceiling <| percentOfDiag (w,h) 0.065) midTop <| plainText ""

bannerText : (Int, Int) -> Element
bannerText (w, h) =
  flow right
    [ container (floor <| toFloat w * 0.85) (ceiling <| percentOfDiag (w, h) 0.060) midLeft
        (toText " Joanna Berkebile"
          |> Text.style (linkStyle w h)
          |> leftAligned)
    , facebookIcon (w,h)
    ]

facebookIcon : (Int, Int) -> Element
facebookIcon (w, h) =
  container (floor <| toFloat w * 0.14) (ceiling <| percentOfDiag (w, h) 0.060) midRight
  <| link "https://www.facebook.com/joanna.carey2"
  <| fittedImage (floor <| percentOfDiag (w,h) 0.055) (floor <| percentOfDiag (w,h) 0.055) "./resources/facebook.png"

banner : (Int, Int) -> Element
banner (w, h) =
  layers
    [ opacity 1.0 <| fittedImage w h "./resources/jo-0.jpg"
    , bannerBg (w, h)
    , bannerText (w, h)
    ]

{-- --------------------------------------------------------------------------------------------------------------------
 -- Utility functions
 ----------------------------------------------------------------------------------------------------------------------}

-- This uses the pythagorean theorem to calculate the the size based on the width/height dimensions and the percentage
-- passed in.  The first parameter is the container dimensions (treated as the width and height of a rectangle) and the
-- second parameter is the percentage of the calculated diagonal to return.
percentOfDiag : (Int, Int) -> Float -> Float
percentOfDiag (winWidth, winHeight) percentage =
  let w = toFloat winWidth
      h = toFloat winHeight
  in (*) percentage << sqrt <| (w ^ 2.0) + (h ^ 2.0)

