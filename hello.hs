import Html

myhtml :: Html
myhtml =
  html_
    "My page title"
    ( h1_ "Welcome to the machine"
        <> ( ul_ [p_ "Let's hack"]
               <> p_ "Let's hack"
           )
    )

main :: IO ()
main = putStrLn (render myhtml)
