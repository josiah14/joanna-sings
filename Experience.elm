import Window

import Website.ContentPageSkeleton (..)

main : Signal Element
main = content "Experience" markdownText <~ Window.dimensions

markdownText : Element
markdownText = [markdown|
## Experience
Joanna has tons of experience.  She sings with Jason Vivone and the Billy Bats, a Kansas City blues and roots band,
and has sung with the East coast opera company Opera Seabrook and was a top student of Robert McFarland, who had a successful
career performing as an Opera Singer.

Obviously, Joanna is awesome and you should take lessons from her *NOW*!!!

_Sign up for voice lessons *NOW*_
|]


