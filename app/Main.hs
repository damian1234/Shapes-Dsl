{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Web.Scotty.Internal.Types
--import Text.Digestive
--import Text.Digestive.Scotty
--import Control.Applicative ((<$>), (<*>))
import Prelude hiding (head)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Text.Blaze.Html.Renderer.Pretty as P
import qualified Text.Blaze.Svg11 as SV11
import qualified Text.Blaze.Svg11.Attributes as SV11A
import Text.Blaze.Svg11 ((!))
import Text.Blaze.Svg.Renderer.Text (renderSvg)
import Data.Text.Lazy
import qualified Shapes as S
import qualified Ansi as A


-- this is the index page when the app is running. it will have a text box with an example already in the text box
-- this example is also the format for a valid drawing which is [[drawing],[drawing]] so you could have 1 or multiple drawings
--get displays the form with the example entered into the text box
-- the post takes the text entered and then uses my Svg descriptor to translate it the input to and Svg
main :: IO ()
main = scotty 3000 $ do
    get "/" $ html $ pack $ myForm

    post "/" $ do
        desc <- param "shapeDesc"
        html $ pack $ P.renderHtml $ do
          (S.bldSvg (read desc))
       
myForm = P.renderHtml $ do
      H.form ! HA.method "POST" $ do
          H.textarea (H.toHtml (show example)) ! HA.name "shapeDesc"
          H.div $ H.button "submit"      

example =  [[((S.translate (S.Vector 200 200)) S.<+>(S.scale (S.Vector 2 2)) S.<+> (S.myRotate 45), (S.square, 50.0, 10.0, 10.0), (A.Blue,(S.boarder A.Black 5)))],[((S.translate (S.Vector 200 500)) S.<+>(S.scale (S.Vector 2 2)), (S.circle, 50.0, 10.0, 10.0), (A.Red,(S.boarder A.Black 5)))]]


