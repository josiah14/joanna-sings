import Window

import Website.ContentPageSkeleton (..)

main : Signal Element
main = content "Voice Coaching" markdownText <~ Window.dimensions

markdownText : Element
markdownText = [markdown|
## Voice Coaching
Joanna just loves voice coaching.  It is all she does,  She would voice coach all day long and
everybody loves her for it.  Take voice lessons from her and be a champion.  Yay Joanna!

_Sign up for voice lessons *NOW*_
|]


