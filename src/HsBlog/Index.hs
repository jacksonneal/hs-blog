module HsBlog.Index where

import HsBlog.Convert (convertStructure)
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex files =
  let previews =
        map
          ( \(file, doc) ->
              case doc of
                Markup.Heading 1 heading : article ->
                  Html.h_ 3 (Html.link_ file (Html.txt_ heading))
                    <> foldMap convertStructure (take 3 article)
                    <> Html.p_ (Html.link_ file (Html.txt_ "..."))
                _ ->
                  Html.h_ 3 (Html.link_ file (Html.txt_ file))
          )
          files
   in Html.html_
        "Blog"
        ( Html.h_ 1 (Html.link_ "index.html" (Html.txt_ "Blog"))
            <> Html.h_ 2 (Html.txt_ "Posts")
            <> mconcat previews
        )
