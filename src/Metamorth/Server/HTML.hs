module Metamorth.Server.HTML
  ( makeMainHTML
  , makeMainHTMLCSS
  , makeMainHTMLCSSOld
  ) where

import Control.Arrow ((***))
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

makeMainHTMLCSS
  :: forall iorth oorth z. (Show iorth, Show oorth, Ord iorth, Ord oorth, Enum iorth, Enum oorth, Bounded iorth, Bounded oorth)
  => FilePath
  -> M.Map String (iorth)
  -> M.Map String (oorth, String)
  -> (z, M.Map String String)
  -> Q [Dec]
makeMainHTMLCSS cssFile mp1 mp2 (_,descMap) = do
  let html1 = makeConverterHTML (Just cssFile) mp1 mp2 descMap
      bs = Utf8.renderHtml html1
  runIO $ checkAndOverwrite bs "static/convert.html"
  return []

makeMainHTMLCSSOld
  :: forall iorth oorth. (Show iorth, Show oorth, Ord iorth, Ord oorth, Enum iorth, Enum oorth, Bounded iorth, Bounded oorth)
  => FilePath
  -> M.Map String (iorth)
  -> M.Map String (oorth, String)
  -> Q [Dec]
makeMainHTMLCSSOld cssFile mp1 mp2 = do
  let html1 = makeConverterHTMLOld (Just cssFile) mp1 mp2
      bs = Utf8.renderHtml html1
  runIO $ checkAndOverwrite bs "static/convert.html"
  return []


makeMainHTML
  :: forall iorth oorth. (Show iorth, Show oorth, Ord iorth, Ord oorth, Enum iorth, Enum oorth, Bounded iorth, Bounded oorth)
  => M.Map String (iorth)
  -> M.Map String (oorth, String)
  -> Q [Dec]
makeMainHTML mp1 mp2 = do
  let html1 = makeConverterHTMLOld Nothing mp1 mp2
      bs = Utf8.renderHtml html1
  runIO $ checkAndOverwrite bs "static/convert.html"
  return []

makeConverterHTMLOld
  :: forall iorth oorth. (Show iorth, Show oorth, Ord iorth, Ord oorth, Enum iorth, Enum oorth, Bounded iorth, Bounded oorth)
  => Maybe FilePath
  -> M.Map String (iorth)
  -> M.Map String (oorth, String)
  -> Html
makeConverterHTMLOld mCssFile iOrths' oOrths' = do
  -- hmm...
  cssHdr

  Html.h1 "Basic Converter"
  Html.br
  makeFormOld iOrths oOrths -- make the basic form...

  Html.script (toHtml @T.Text "") ! Atr.src "run_convert.js"

  where
    iOrths = revMap iOrths'
    oOrths = revMap $ fst <$> oOrths'
    cssHdr :: Html
    cssHdr
      | (Just cssFile) <- mCssFile
      = Html.header $ Html.link ! Atr.rel "stylesheet" ! Atr.href (sv cssFile)
      | otherwise = return ()


makeConverterHTML
  :: forall iorth oorth. (Show iorth, Show oorth, Ord iorth, Ord oorth, Enum iorth, Enum oorth, Bounded iorth, Bounded oorth)
  => Maybe FilePath
  -> M.Map String (iorth)
  -> M.Map String (oorth, String)
  -> M.Map String String
  -> Html
makeConverterHTML mCssFile iOrths' oOrths' dscMap = do
  -- hmm...
  cssHdr

  Html.h1 "Basic Converter"
  Html.br
  makeForm iOrths oOrths -- make the basic form...

  Html.script (toHtml @T.Text "") ! Atr.src "run_convert.js"

  where
    iOrths = revMapNew dscMap           iOrths'
    oOrths = revMapNew dscMap $ fst <$> oOrths'
    -- xOrths = revMapNew dscMap oOrths'
    cssHdr :: Html
    cssHdr
      | (Just cssFile) <- mCssFile
      = Html.header $ Html.link ! Atr.rel "stylesheet" ! Atr.href (sv cssFile)
      | otherwise = return ()


revMap 
  :: forall orth. (Show orth, Ord orth, Enum orth, Bounded orth)
  => M.Map String orth
  -> M.Map T.Text T.Text
revMap orthMap = M.map T.pack $ M.mapKeys stripStart $ M.mapMaybe S.lookupMin $ invertOrthMap orthMap

revMap'
  :: forall orth. (Show orth, Ord orth, Enum orth, Bounded orth)
  => M.Map String (orth, String)
  -> M.Map T.Text (T.Text, T.Text)
revMap' orthMap = M.map (T.pack *** T.pack) $ M.mapKeys stripStart $ mapMaybeFst S.lookupMin $ invertOrthMapAlt orthMap

revMapNew
  :: forall orth. (Show orth, Ord orth, Enum orth, Bounded orth)
  => M.Map String String
  -> M.Map String orth
  -> M.Map T.Text (T.Text, T.Text)
revMapNew dscMap orthMap = M.map (T.pack *** T.pack) $ M.mapKeys stripStart $ mapMaybeFst S.lookupMin $ invertOrthMapNew dscMap orthMap

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

sv :: String -> AttributeValue
sv = stringValue

-- Note: This is a different organisation of the
-- Maps from the main function.
-- makeForm :: M.Map T.Text T.Text -> M.Map T.Text T.Text -> Html
makeForm :: M.Map T.Text (T.Text, T.Text) -> M.Map T.Text (T.Text, T.Text) -> Html
makeForm iOrths oOrths = do
  Html.form (do
    text "Input Orthography: "
    Html.br
    radioButtons'' False True 1 "input" iOrths
    Html.br
    text "Output Orthography: "
    Html.br
    radioButtons'' False False 2 "output" oOrths
    Html.br
    Html.label ("Input Text") ! Atr.for "text_box"
    Html.br
    Html.textarea (toHtml @T.Text "") ! Atr.id "text_box" ! Atr.name "text" ! Atr.rows "6" ! Atr.cols "40" ! Atr.placeholder "Input Text Here"
    Html.br
    ) ! Atr.name "mainForm" ! Atr.id "mainForm"
  Html.button ("Convert") ! Atr.onclick (tv convertScript)
  Html.br
  Html.label ("Output Text") ! Atr.for "outbox"
  Html.br
  Html.textarea (toHtml @T.Text "") ! Atr.readonly "true" ! Atr.name "output_area" ! Atr.rows "6" ! Atr.cols "40" ! Atr.id "outbox" ! Atr.placeholder "Output Text Here"

-- Note: This is a different organisation of the
-- Maps from the main function.
-- makeForm :: M.Map T.Text T.Text -> M.Map T.Text T.Text -> Html
makeFormOld :: M.Map T.Text T.Text -> M.Map T.Text T.Text -> Html
makeFormOld iOrths oOrths = do
  Html.form (do
    text "Input Orthography: "
    Html.br
    radioButtons 1 "input" iOrths
    Html.br
    text "Output Orthography: "
    Html.br
    radioButtons 2 "output" oOrths
    Html.br
    Html.label ("Input Text") ! Atr.for "text_box"
    Html.br
    Html.textarea (toHtml @T.Text "") ! Atr.id "text_box" ! Atr.name "text" ! Atr.rows "6" ! Atr.cols "40" ! Atr.placeholder "Input Text Here"
    Html.br
    ) ! Atr.name "mainForm" ! Atr.id "mainForm"
  Html.button ("Convert") ! Atr.onclick (tv convertScript)
  Html.br
  Html.label ("Output Text") ! Atr.for "outbox"
  Html.br
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

radioButtons :: Int -> T.Text -> M.Map T.Text T.Text -> Html
radioButtons = radioButtons' False

-- Need the extra 'Int' value to distinguish between the radio
-- buttons for the input option and the radio buttons for the
-- output option.
-- Note that the *value* must still have the original value.
radioButtons' :: Bool -> Int -> T.Text -> M.Map T.Text T.Text -> Html
radioButtons' addBr n radioName theOptions = Html.div (forM_ (M.assocs theOptions) $ \(txtShow, txtSend) -> do
  let txtSend' = txtSend <> T.pack (show n)
  Html.input ! Atr.type_ "radio" ! Atr.id (tv txtSend') ! Atr.name (tv radioName) ! Atr.value (tv txtSend)
  Html.label (text txtShow) ! Atr.for (tv txtSend')
  when addBr Html.br
  ) ! Atr.class_ "radio-in"

-- Trying to make one that gives a tooltip for that
-- particular orthography.
radioButtons'' :: Bool -> Bool -> Int -> T.Text -> M.Map T.Text (T.Text, T.Text) -> Html
radioButtons'' addBr abvBlw n radioName theOptions = Html.div (forM_ (zip [1..] (M.assocs theOptions)) $ \(idx, (txtShow, (txtSend, txtInfo))) -> do
  let txtSend' = txtSend <> T.pack (show n)
      txtInfo' = if (txtInfo == "") then ("Couldn't find description") else txtInfo
  Html.input ! Atr.type_ "radio" ! Atr.id (tv txtSend') ! Atr.name (tv radioName) ! Atr.value (tv txtSend)
  Html.label (text txtShow <> do
       Html.span (text txtInfo') ! Atr.class_ "tooltiptext"
    ) ! Atr.for (tv txtSend') ! Atr.class_ ("tooltip " <> toolPos <> (if idx == 1 then " toolfirst" else ""))
  when addBr Html.br
  ) ! Atr.class_ "radio-in"
  where 
    toolPos 
      | abvBlw    = "toolabove"
      | otherwise = "toolbelow"

{-
  <input type="radio" id="html" name="fav_language" value="HTML">
  <label for="html">HTML</label><br>
  <input type="radio" id="css" name="fav_language" value="CSS">
  <label for="css">CSS</label><br>
  <input type="radio" id="javascript" name="fav_language" value="JavaScript">
  <label for="javascript">JavaScript</label>
-}