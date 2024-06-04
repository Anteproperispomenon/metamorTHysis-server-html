module Metamorth.Server.HTML
  (

  ) where

import Control.Monad

import Data.Map.Strict qualified as M

import Data.Text qualified as T

import Text.Blaze.Html
import Text.Blaze.Html5            qualified as Html
import Text.Blaze.Html5.Attributes qualified as Atr

makeConverterHTML
  :: forall iorth oorth. (Show iorth, Show oorth, Ord iorth, Ord oorth, Enum iorth, Enum oorth, Bounded iorth, Bounded oorth)
  => M.Map String (iorth)
  -> M.Map String (oorth, String)
  -> Html
makeConverterHTML iOrths oOrths = do


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
    Html.textarea (toHtml "Input Text Here") ! Atr.id_ "text_box" ! Atr.name "text" ! Atr.rows 6 ! Atr.columns 40
    Html.br
    ) ! Atr.name "mainForm"
  Html.button ("Convert(?)") ! Atr.onClick (convertScript)
  Html.textarea (toHtml "Output Text Here") ! Atr.readonly "true" ! Atr.name "outbox" ! Atr.rows 6 ! Atr.columns 40

convertScript :: T.Text
convertScript = mappend
    [ "var jsonInput = JSON.stringify(Object.fromEntries(mainForm));"
    , "var pasteResult = function(txt) {"
    , " "
    , " }; "
    , "postConvert(jsonInput, );"
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