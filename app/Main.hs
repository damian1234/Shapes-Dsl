{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

--import Text.Digestive
--import Text.Digestive.Scotty
--import Control.Applicative ((<$>), (<*>))
import Prelude hiding (head)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes 
import qualified Text.Blaze.Html.Renderer.Pretty as P
import Data.Text.Lazy
import qualified Ansi as A
import qualified Shapes as S

serve = scotty 3000 $ get "/" $ html $ pack $ P.renderHtml render 

render = do
  H.head $ H.title "Assignment"
  H.body $ do
    H.h1 "Shape"
    S.bldSvg [[((S.translate (S.Vector 200 200)) S.<+>(S.scale (S.Vector 2 2)) S.<+> (S.myRotate 45), (S.square, 50.0, 10.0, 10.0), (A.Blue,(S.boarder A.Black 5)))],[((S.translate (S.Vector 200 500)) S.<+>(S.scale (S.Vector 2 2)), (S.circle, 50.0, 10.0, 10.0), (A.Red,(S.boarder A.Black 5)))]]



--  S.svgdesc [((S.rotate 45),S.square, [(A.Blue,(S.boarder A.Black 5))])]

main = serve

{-data FormData = FormData { field1 :: String
                         , field2 :: Integer
                         }

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
        setHeader "Content-Type" "text/html"
        raw " <html><body><form enctype='multipart/form-data' method='post'> \
              \ <input type='text' name='test-form.field1'/> \
              \ <input type='text' name='test-form.field2'/> \
              \ <input type='submit'/> \
              \ </form></body></html>"

    post "/" $ do
 -}
