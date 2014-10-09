import Maybe
import String
import Text
import Window

main = content <~ Window.dimensions

linkStyle : Int -> Int -> Text.Style
linkStyle w h = { typeface = ["century gothic", "sans-serif"]
                , height = Just <| percentOfDiag (w,h) 0.045
                , color = white
                , bold = True
                , italic = True
                , line = Nothing
                }

sectionTextStyle dims = { typeface = ["century gothic", "sans-serif"]
                        , height = Just <| sectionTextHeight dims
                        , color = black
                        , bold = True
                        , italic = False
                        , line = Nothing
                        }

content : (Int, Int) -> Element
content (w, h) =
  let thisBanner        = banner (w, h)
      heightMinusBanner = h - (heightOf <| thisBanner)
      mainSplit         = floor <| (if w > h then toFloat w * 0.618 else toFloat h * 0.382) -- golden ratio
      -- I would move this into the firstSection method if it didn't cause glitches when resizing the window to points
      -- near the transition between portrait and landscape.
      subSplit          = floor <| toFloat (if w > h then heightMinusBanner else w) * 0.618
      body              = let flowDirection = if w > h then right else down
                          in flow flowDirection
                               [ firstSection (w, h) (mainSplit, subSplit)
                               , contactSection (w, h) mainSplit heightMinusBanner
                               ]
  in flow down [ thisBanner, body ]

sectionTextHeight : (Int, Int) -> Float
sectionTextHeight (w, h) = percentOfDiag (w, h) 0.09

sectionText : (Int, Int) -> String -> Element
sectionText dims str = centered <| Text.style (sectionTextStyle dims) <| toText str

contactSection : (Int, Int) -> Int -> Int -> Element
contactSection (winWidth, winHeight) splitWidth heightMinusBanner =
  let sectionWidth  = if winWidth > winHeight then winWidth - splitWidth else winWidth
      sectionHeight = if winWidth > winHeight
                      then floor <| toFloat heightMinusBanner
                      else heightMinusBanner - splitWidth
  in color yellow <| container sectionWidth sectionHeight middle <| sectionText (sectionWidth, sectionHeight) "Contact"

firstSection : (Int, Int) -> (Int, Int) -> Element
firstSection (winWidth, winHeight) (subSectionLength, subSectionBreadth) =
  let flowDirection     = if winWidth > winHeight then down else right
      subSectionWidth   = if winWidth > winHeight then subSectionLength else subSectionBreadth
      subSectionHeight  = if winWidth > winHeight then subSectionBreadth else subSectionLength
      subSectionWidth1  = if winWidth > winHeight
                          then subSectionLength
                          else ceiling <| toFloat subSectionBreadth / 1.618
      subSectionHeight1 = if winWidth > winHeight
                          then ceiling <| toFloat subSectionBreadth / 1.618
                          else subSectionLength
  in flow flowDirection
       [ voiceCoachingSection (winWidth, winHeight) (subSectionWidth, subSectionHeight)
       , experienceSection (winWidth, winHeight) (subSectionWidth1, subSectionHeight1)
       ]

subSection : (Int, Int) -> (Int, Int) -> String -> Color -> Element
subSection (winWidth, winHeight) (sectionWidth, sectionHeight) label bgColor =
  let textHeight = sectionTextHeight (sectionWidth, sectionHeight)
  in if winWidth > winHeight
     then color bgColor <| container sectionWidth sectionHeight middle
            <| sectionText (sectionWidth, sectionHeight) label
     else
       let labelParts = String.split " " label
       in color bgColor <| container sectionWidth sectionHeight middle <| flow down
            <| map (\str -> container sectionWidth (ceiling <| textHeight + 5) middle
                              <| sectionText (sectionWidth, sectionHeight) str)
                   labelParts

voiceCoachingSection : (Int, Int) -> (Int, Int) -> Element
voiceCoachingSection (winWidth, winHeight) (sectionWidth, sectionHeight) =
  subSection (winWidth, winHeight) (sectionWidth, sectionHeight) "Voice Coaching" red

experienceSection : (Int, Int) -> (Int, Int) -> Element
experienceSection (winWidth, winHeight) (sectionWidth, sectionHeight) =
  subSection (winWidth, winHeight) (sectionWidth, sectionHeight) "Experience" blue

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
  in (*) percentage <| sqrt <| (w ^ 2.0) + (h ^ 2.0)

