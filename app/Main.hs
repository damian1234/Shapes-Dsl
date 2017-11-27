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
--serve = scotty 3000 $ get "/" $ html $ pack $ P.renderHtml render 

render:: [S.Drawing] -> H.Html 
render d = do
  H.head $ H.title "Assignment"
  H.body $ do
    H.h1 "Shape"




example =  [[((S.translate (S.Vector 200 200)) S.<+>(S.scale (S.Vector 2 2)) S.<+> (S.myRotate 45), (S.square, 50.0, 10.0, 10.0), (A.Blue,(S.boarder A.Black 5)))],[((S.translate (S.Vector 200 500)) S.<+>(S.scale (S.Vector 2 2)), (S.circle, 50.0, 10.0, 10.0), (A.Red,(S.boarder A.Black 5)))]]



--  S.svgdesc [((S.rotate 45),S.square, [(A.Blue,(S.boarder A.Black 5))])]



main :: IO ()
main = scotty 3000 $ do
    get "/" $ html $ pack $ myForm

    post "/" $ do
        desc <- param "shapeDesc"
        html $ pack $ P.renderHtml $ do
          (S.bldSvg (read desc))
       -- sh <- param "shape"
       -- st <- param "style"
       
myForm = P.renderHtml $ do
      H.form ! HA.method "POST" $ do
          H.textarea (H.toHtml (show example)) ! HA.name "shapeDesc"
          H.div $ H.button "submit"      
-- let shape = sh::String
       -- let style = st::String    
--    get "/shapes" $ do 
      --get "/shapes" $ do
      --    html $ pack $ P.renderHtml (S.bldSvg [[(transform, (S.circle, 50.0, 10.0, 10.0), (A.Red,(S.boarder A.Black 5)))]] )
      --    where t <- param "transform"
       --       let trans = t::String  --Transform
       --       let transform = read trans::S.Transform

    --    S.bldSvg[transform, shape, style]       
--    get "/shapes" $ html $ pack $ P.renderHtml render    
--[trans, shape, style]
