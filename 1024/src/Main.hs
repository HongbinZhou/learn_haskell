import Graphics.Vty.Widgets.All
import qualified Data.Text as T

-- ref: https://github.com/jtdaugherty/vty-ui/blob/master/doc/ch1/getting_started.tex
main :: IO ()
main = do
   e <- editWidget
   ui <- centered e

   fg <- newFocusGroup
   addToFocusGroup fg e

   c <- newCollection
   addToCollection c ui fg

   e `onActivate` \this ->
     getEditText this >>= (error . ("You entered: " ++) . T.unpack)

   runUi c defaultContext
