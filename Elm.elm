import Maybe
import Text
import Window

linkStyle : Int -> Int -> Text.Style
linkStyle w h = { typeface = ["century gothic", "sans-serif"]
                , height = Just <| percentOfDiag (w,h) 0.045
                , color = white
                , bold = True
                , italic = True
                , line = Nothing
                }

main = content <~ Window.dimensions

content : (Int, Int) -> Element
content (w, h) =
  let heightMinusBanner = h - (heightOf <| banner (w,h))
      mainSplitPosition = floor <| toFloat (if w > h then w else h) * (if w > h then 0.618 else 0.382)
      secondarySplitPosition = (if w > h then heightMinusBanner else w) // 2
      body =
        if w > h
        then flow right
               [ flow down
                   [ color red <| container mainSplitPosition secondarySplitPosition middle <| plainText "Voice Coaching"
                   , color blue <| container  mainSplitPosition secondarySplitPosition middle <| plainText "Experience"
                   ]
               , color yellow <| container  (w - mainSplitPosition) (floor <| toFloat heightMinusBanner) middle <| plainText "Contact"
               ]
        else flow down
               [ flow right
                   [ color red <| container  secondarySplitPosition mainSplitPosition middle <| plainText "Voice Coaching"
                   , color blue <| container  secondarySplitPosition mainSplitPosition middle <| plainText "Experience"
                   ]
               , color yellow <| container  w (heightMinusBanner - mainSplitPosition) middle <| plainText "Contact"
               ]
  in flow down
       [ banner (w,h)
       , body
       ]

bannerBg : (Int, Int) -> Element
bannerBg (w, h) = opacity 1 <| color black <| spacer w (ceiling <| percentOfDiag (w,h) 0.075)

bannerText : (Int, Int) -> Element
bannerText (w, h) =
  let textWidth = (floor <| toFloat w * 0.88)
  in flow right
       [ container textWidth (ceiling <| percentOfDiag (w, h) 0.060) midLeft
           (toText " Joanna Berkebile"
             |> Text.style (linkStyle w h)
             |> leftAligned)
       , facebookIcon (w,h)
       ]

facebookIcon : (Int, Int) -> Element
facebookIcon (w, h) =
  container (floor <| toFloat w * 0.115) (ceiling <| percentOfDiag (w, h) 0.070) midRight
       <| link "https://www.facebook.com/joanna.carey2"
       <| image (floor <| percentOfDiag (w,h) 0.060) (floor <| percentOfDiag (w,h) 0.060) "./resources/facebook.png"

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

