import Maybe
import Text
import Window

linkStyle : Int -> Int -> Text.Style
linkStyle w h = { typeface = ["century gothic", "sans-serif"]
                , height = Just <| percentOfDiag (w,h) 0.042
                , color = white
                , bold = True
                , italic = True
                , line = Nothing
                }

main = content <~ Window.dimensions

content : (Int, Int) -> Element
content (w, h) =
  let heightMinusBanner = h - (heightOf <| banner (w,h))
      widthLeftSplit = floor <| toFloat heightMinusBanner / 0.809 -- golden ratio
      heightBody = heightMinusBanner
  in flow down
       [ banner (w,h)
       , flow right
           [ flow down
               [ color red <| spacer widthLeftSplit <| heightBody // 2
               , color blue <| spacer widthLeftSplit <| heightBody // 2
               ]
           , color yellow <| spacer (w - widthLeftSplit) (floor <| toFloat heightMinusBanner)
           ]
       ]

bannerBg : (Int, Int) -> Element
bannerBg (w, h) = opacity 1 <| color black <| spacer w (ceiling <| percentOfDiag (w,h) 0.065)

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
    [ bannerBg (w, h)
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

