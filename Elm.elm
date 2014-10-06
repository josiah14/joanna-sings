import Maybe
import Text
import Window

main = content <~ Window.dimensions

linkStyle : Int -> Int -> Text.Style
linkStyle w h = { typeface = ["Tahoma"]
                , height = Just ((sqrt <| (toFloat h ^ 2.0) + (toFloat w ^ 2.0)) / 35.0)
                , color = white
                , bold = True
                , italic = False
                , line = Nothing
                }

banner : (Int, Int) -> Element
banner (w, h) = opacity 0.5 <| color black <| container w (ceiling (( sqrt <| (toFloat h ^ 2.0) + (toFloat w ^ 2.0)) / 15)) midTop <| plainText ""

bannerText : (Int, Int) -> Element
bannerText (w, h) = flow down
                      [ container w (ceiling (( sqrt <| (toFloat h ^ 2.0) + (toFloat w ^ 2.0)) / 33.0)) midTop (toText "Joanna Berkebile"
                          |> Text.style (linkStyle w h)
                          |> centered)
                      , container w (ceiling (( sqrt <| (toFloat h ^ 2.0) + (toFloat w ^ 2.0)) / 25.0)) middle (toText "Professional live singer and voice instructor"
                          |> Text.color white
                          |> Text.height ((sqrt <| (toFloat h ^ 2.0) + (toFloat w ^ 2.0)) / 45.0)
                          |> centered)
                      ]

content : (Int, Int) -> Element
content (w, h) =
  layers
    [ opacity 1.0 <| fittedImage w h "./resources/jo-0.jpg"
    , banner (w, h)
    , bannerText (w, h)
    ]

