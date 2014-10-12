import Window

import Website.ContentPageSkeleton (..)

main : Signal Element
main = content "Experience" markdownText <~ Window.dimensions

markdownText : Element
markdownText = [markdown|
## Experience
Joanna lives in Kansas City and her phone number is 814-555-5555.

her e-mail is joanna.berkebile@joannaisawesome.com
|]


