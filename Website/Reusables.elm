module Website.Reusables where

import Text
import String

headerStyle : Int -> Int -> Text.Style
headerStyle w h = { typeface = ["century gothic", "sans-serif"]
                  , height = Just <| percentOfDiag (w,h) 0.042
                  , color = white
                  , bold = True
                  , italic = True
                  , line = Nothing
                  }

footerStyle : Int -> Int -> Text.Style
footerStyle w h = { typeface = ["century gothic", "sans-serif"]
                  , height = Just <| percentOfDiag (w,h) 0.03
                  , color = black
                  , bold = True
                  , italic = False
                  , line = Nothing
                  }

bannerBg : (Int, Int) -> Element
bannerBg (w, h) = opacity 1 <| color black <| spacer w (ceiling <| percentOfDiag (w,h) 0.070)

bannerText : (Int, Int) -> Element
bannerText (w, h) =
  let textWidth = (floor <| toFloat w * 0.87)
  in flow right
       [ container textWidth (ceiling <| percentOfDiag (w, h) 0.060) midLeft
           (toText " Joanna Berkebile"
             |> Text.style (headerStyle w h)
             |> leftAligned)
       , facebookIcon (w,h)
       ]

facebookIcon : (Int, Int) -> Element
facebookIcon (w, h) =
  container (floor <| toFloat w * 0.120) (ceiling <| percentOfDiag (w, h) 0.070) midRight
       <| link "https://www.facebook.com/joanna.carey2"
       <| image (floor <| percentOfDiag (w,h) 0.058) (floor <| percentOfDiag (w,h) 0.058) "./resources/facebook.png"

banner : (Int, Int) -> Element
banner (w, h) =
  link "Elm.elm"
  <| layers
       [ bannerBg (w, h)
       , bannerText (w, h)
       ]

footer : (Int, Int) -> String -> String -> Element
footer (w, h) leftText rightText =
  let percentage = if w > h then 0.07 else 0.12
      footerHeight = floor <| percentOfDiag (w,h) percentage
  in opacity 1 <| flow right
                    [ link ((String.join "" <| String.split " " leftText) ++ ".elm")
                        <| color red <| container (w // 2) footerHeight middle
                          <| centered
                          <| Text.style (footerStyle w h)
                          <| toText leftText
                    , link ((String.join "" <| String.split " " rightText) ++ ".elm")
                        <| color blue <| container (w // 2) footerHeight middle
                          <| centered
                          <| Text.style (footerStyle w h)
                          <| toText rightText
                    ]
  --in opacity 1 <| color black <| spacer w (floor <| percentOfDiag (w,h) percentage)

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
  in (*) percentage <| sqrt <| (w ^ 2.0) + (h ^ 2.0)

