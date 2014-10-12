module Website.ContentPageSkeleton where

import Website.Reusables (..)

-- window.dimensions -> markdown content -> resulting page element
content : String -> Element -> (Int, Int) -> Element
content currentPage markdownText (w, h) =
  let pageHeader = banner (w, h)
      pageFooter =
        let leftText  = case currentPage of
                          "Voice Coaching" -> "Experience"
                          _                -> "Voice Coaching"
            rightText = case currentPage of
                          "Contact" -> "Experience"
                          _         -> "Contact"
        in footer (w, h) leftText rightText
      bodyHeight = h - heightOf pageHeader - heightOf pageFooter
      body (w, h) =
        container w h middle <| container (floor <| toFloat w * 0.80) h middle (width (floor <| toFloat w * 0.80) markdownText)
  in flow down
       [ pageHeader
       , body (w, bodyHeight)
       , pageFooter
       ]

