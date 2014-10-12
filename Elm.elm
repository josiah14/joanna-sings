import Maybe
import String
import Text
import Window

-- local imports
import Website.Reusables (..)

main : Signal Element
main = content <~ Window.dimensions


sectionTextStyle dims = { typeface = ["century gothic", "sans-serif"]
                        , height = Just <| sectionTextHeight dims
                        , color = black
                        , bold = True
                        , italic = False
                        , line = Nothing
                        }

sectionTextHeight : (Int, Int) -> Float
sectionTextHeight (w, h) = percentOfDiag (w, h) 0.075

content : (Int, Int) -> Element
content (w, h) =
  let thisBanner        = banner (w, h)
      heightMinusBanner = h - (heightOf <| thisBanner)
      mainSplit         = floor <| (if w > heightMinusBanner then toFloat w * 0.618 else toFloat h * 0.382) -- golden ratio
      body              = let flowDirection = if w > heightMinusBanner then right else down
                          in flow flowDirection
                               [ firstSection (w, heightMinusBanner) mainSplit
                               , contactSection (w,heightMinusBanner) mainSplit
                               ]
  in flow down [ thisBanner, body ]

sectionText : (Int, Int) -> String -> Element
sectionText dims str = centered <| Text.style (sectionTextStyle dims) <| toText str

contactSection : (Int, Int) -> Int -> Element
contactSection (winWidth, winHeight) splitWidth =
  let sectionWidth  = if winWidth > winHeight then winWidth - splitWidth else winWidth
      sectionHeight = if winWidth > winHeight
                      then winHeight
                      else winHeight - splitWidth
  in link "Contact.elm"
     <| color yellow <| container sectionWidth sectionHeight middle
     <| sectionText (sectionWidth, sectionHeight) "Contact"

firstSection : (Int, Int) -> Int -> Element
firstSection (winWidth, winHeight) subSectionLength =
  let flowDirection     = if winWidth > winHeight then down else right
      subSectionBreadth = floor <| toFloat (if winWidth > winHeight then winHeight else winWidth) * 0.618
      subSectionWidth   = if winWidth > winHeight then subSectionLength else subSectionBreadth
      subSectionHeight  = if winWidth > winHeight then subSectionBreadth else subSectionLength
      subSectionWidth1  = if winWidth > winHeight
                          then subSectionLength
                          else winWidth - subSectionBreadth
      subSectionHeight1 = if winWidth > winHeight
                          then winHeight - subSectionBreadth
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
  link "VoiceCoaching.elm"
  <| subSection (winWidth, winHeight) (sectionWidth, sectionHeight) "Voice Coaching" red

experienceSection : (Int, Int) -> (Int, Int) -> Element
experienceSection (winWidth, winHeight) (sectionWidth, sectionHeight) =
  link "Experience.elm"
  <| subSection (winWidth, winHeight) (sectionWidth, sectionHeight) "Experience" blue

