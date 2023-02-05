module HsBlog.Convert where

import qualified HsBlog.Html
import qualified HsBlog.Markup

convertStructure :: HsBlog.Markup.Structure -> HsBlog.Html.Structure
convertStructure structure =
  case structure of
    HsBlog.Markup.Heading n txt ->
      HsBlog.Html.h_ n txt
    HsBlog.Markup.Paragraph p ->
      HsBlog.Html.p_ p
    HsBlog.Markup.UnorderedList list ->
      HsBlog.Html.ul_ $ map HsBlog.Html.p_ list
    HsBlog.Markup.OrderedList list ->
      HsBlog.Html.ol_ $ map HsBlog.Html.p_ list
    HsBlog.Markup.CodeBlock list ->
      HsBlog.Html.code_ (unlines list)

convert :: HsBlog.Html.Title -> HsBlog.Markup.Document -> HsBlog.Html.Html
convert title = HsBlog.Html.html_ title . foldMap convertStructure
