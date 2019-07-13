{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE MonoLocalBinds #-}
module Reflex.MDL.Text where

import "base"            Data.Maybe (isJust, fromJust)
import "base"            Data.Monoid ((<>))
import "base"            Control.Monad.IO.Class (liftIO)
import "text"            Data.Text (Text)
import "reflex-dom"      Reflex.Dom
import "ghcjs-base"      GHCJS.Types
import "ghcjs-dom-jsffi" GHCJS.DOM.Types (unElement)
import "ghcjs-dom-jsffi" GHCJS.DOM.Element (IsElement, toElement)

mdlText :: MonadWidget t m
        => Text
        -- ^ HTML id
        -> Text
        -- ^ input type
        -> Event t Text
        -- ^ External set-value event
        -> Dynamic t Text
        -- ^ Label text
        -> Maybe (Dynamic t Text)
        -- ^ Pattern text
        -> Maybe (Dynamic t Text)
        -- ^ Error text
        -> m (Dynamic t Text)
        -- ^ Output text
mdlText htmlId inputType setValueE labelD mPatternD mErrorD = do
    (element_, textD) <- elAttr' "el" ( "class" =: "mdl-textfield mdl-js-textfield mdl-textfield--floating-label"
                                      <> "id" =: htmlId
                                       ) $ do
        let attrPatternD = case mPatternD of
                           Nothing       -> constDyn mempty
                           Just patternD -> ("pattern" =:) <$> patternD

        -- input
        let inputAttrsD = (( "class" =: "mdl-textfield__input"
                          <> "id" =: (htmlId <> "__input")) <>
                          ) <$> attrPatternD
        let textInputConfig = TextInputConfig
                            { _textInputConfig_inputType    = inputType
                            , _textInputConfig_initialValue = ""
                            , _textInputConfig_setValue     = setValueE
                            , _textInputConfig_attributes   = inputAttrsD
                            }
        textD <- _textInput_value <$> textInput textInputConfig

        -- label
        elAttr "label" ( "class" =: "mdl-textfield__label"
                      <> "for"   =: htmlId
                       ) $ dynText labelD

        -- error message
        if (isJust mErrorD) && (isJust mPatternD) then do
            elAttr "span" ("class" =: "mdl-textfield__error") $ dynText $ fromJust mErrorD
        else blank

        return textD

    -- handle setValue event - the delay is needed so the element will be created before
    -- the checkDirty is called
    checkDirtyE <- delay 0.1 setValueE
    performEvent_ $ ffor checkDirtyE $ \_ -> liftIO $ setFocused (_element_raw element_)

    return textD


setFocused :: (IsElement element)
           => element
           -- ^ Element to bind to
           -> IO ()
setFocused element_ = do
    let js_element = unElement . toElement $ element_
    js_setFocused js_element


foreign import javascript unsafe
    "(function() {                                            \
    \    if ($1 === undefined) { return; }                    \
    \    if ($1.MaterialTextfield === undefined) { return; }  \
    \    $1.MaterialTextfield.checkDirty();                   \
    \})()"
    js_setFocused :: JSVal
                  -- ^ Element to use
                  -> IO ()


emptyMdlText :: MonadWidget t m
             => m (Dynamic t Text)
             -- ^ Output text
emptyMdlText = mdlText ""
                       ""
                       never
                       (constDyn "")
                       Nothing
                       Nothing
