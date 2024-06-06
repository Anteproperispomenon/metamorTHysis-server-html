module Metamorth.Server.HTML
  ( makeMainHTML
  ) where

import Control.Monad

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

import Data.Text qualified as T

import Text.Blaze.Html
import Text.Blaze.Html5            qualified as Html
import Text.Blaze.Html5.Attributes qualified as Atr

import Metamorth.Server.HTML.Extra
import Metamorth.Server.HTML.FilePath

import Language.Haskell.TH

import Text.Blaze.Html.Renderer.Utf8 qualified as Utf8

makeMainHTML
  :: forall iorth oorth. (Show iorth, Show oorth, Ord iorth, Ord oorth, Enum iorth, Enum oorth, Bounded iorth, Bounded oorth)
  => M.Map String (iorth)
  -> M.Map String (oorth, String)
  -> Q [Dec]
makeMainHTML mp1 mp2 = do
  let html1 = makeConverterHTML mp1 mp2
      bs = Utf8.renderHtml html1
  runIO $ checkAndOverwrite bs "static/convert.html"
  return []
    

makeConverterHTML
  :: forall iorth oorth. (Show iorth, Show oorth, Ord iorth, Ord oorth, Enum iorth, Enum oorth, Bounded iorth, Bounded oorth)
  => M.Map String (iorth)
  -> M.Map String (oorth, String)
  -> Html
makeConverterHTML iOrths' oOrths' = do
  -- hmm...
  Html.h1 "Basic Converter"
  Html.br
  makeForm iOrths oOrths -- make the basic form...

  Html.script (toHtml @T.Text "") ! Atr.src "run_convert.js"

  where
    iOrths = revMap iOrths'
    oOrths = revMap $ fst <$> oOrths'

revMap 
  :: forall orth. (Show orth, Ord orth, Enum orth, Bounded orth)
  => M.Map String orth
  -> M.Map T.Text T.Text
revMap orthMap = M.map T.pack $ M.mapKeys stripStart $ M.mapMaybe S.lookupMin $ invertOrthMap orthMap

stripStart :: (Show a) => a -> T.Text
stripStart x
  | (Just txt') <- T.stripPrefix "In"  txt
  = txt'
  | (Just txt') <- T.stripPrefix "Out" txt
  = txt'
  | otherwise = txt
  where txt = T.pack $ show x


tv :: T.Text -> AttributeValue
tv = textValue

-- Note: This is a different organisation of the
-- Maps from the main function.
makeForm :: M.Map T.Text T.Text -> M.Map T.Text T.Text -> Html
makeForm iOrths oOrths = do
  Html.form (do
    text "Input Orthography: "
    radioButtons "input" iOrths
    Html.br
    text "Output Orthography: "
    radioButtons "output" oOrths
    Html.br
    Html.label ("Input Text") ! Atr.for "text_box"
    Html.textarea (toHtml @T.Text "") ! Atr.id "text_box" ! Atr.name "text" ! Atr.rows "6" ! Atr.cols "40" ! Atr.placeholder "Input Text Here"
    Html.br
    ) ! Atr.name "mainForm" ! Atr.id "mainForm"
  Html.button ("Convert(?)") ! Atr.onclick (tv convertScript)
  Html.textarea (toHtml @T.Text "") ! Atr.readonly "true" ! Atr.name "output_area" ! Atr.rows "6" ! Atr.cols "40" ! Atr.id "outbox" ! Atr.placeholder "Output Text Here"

-- Need to check what fields are in the returned text.
convertScript :: T.Text
convertScript = mconcat
    [ "var myForm = document.getElementById('mainForm');"
    , "var thisForm = new FormData(myForm);"
    , "var jsonInput = Object.fromEntries(thisForm);"
    -- , "var jsonInput = JSON.stringify(Object.fromEntries(thisForm));"
    , "var pasteResult = function(rslt) {"
    , "document.getElementById('outbox').innerHTML = rslt.text;"
    , " }; "
    , "var pasteError = function(rslt) {"
    , "document.getElementById('outbox').innerHTML = rslt;"
    , "};"
    , "postConvert(jsonInput, pasteResult, pasteError);"
    ]
    

-- from https://stackoverflow.com/questions/41431322/how-to-convert-formdata-html5-object-to-json
-- JSON.stringify(Object.fromEntries(mainForm));

radioButtons :: T.Text -> M.Map T.Text T.Text -> Html
radioButtons = radioButtons' False

radioButtons' :: Bool -> T.Text -> M.Map T.Text T.Text -> Html
radioButtons' addBr radioName theOptions = forM_ (M.assocs theOptions) $ \(txtShow, txtSend) -> do
  Html.input ! Atr.type_ "radio" ! Atr.id (tv txtSend) ! Atr.name (tv radioName) ! Atr.value (tv txtSend)
  Html.label (text txtShow) ! Atr.for (tv txtSend)
  when addBr Html.br

{-
  <input type="radio" id="html" name="fav_language" value="HTML">
  <label for="html">HTML</label><br>
  <input type="radio" id="css" name="fav_language" value="CSS">
  <label for="css">CSS</label><br>
  <input type="radio" id="javascript" name="fav_language" value="JavaScript">
  <label for="javascript">JavaScript</label>
-}