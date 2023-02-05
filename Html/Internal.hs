module Html.Internal where

newtype Html = Html String

newtype Structure = Structure String

instance Semigroup Structure where
  (<>) c1 c2 =
    Structure (getStructureString c1 <> getStructureString c2)

type Title = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title (Structure content) = Html (el "html" (el "head" (el "title" title) <> el "body" content))

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

-- * Render

render :: Html -> String
render (Html x) = x

-- * Util

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString (Structure content) = content

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in concatMap escapeChar
