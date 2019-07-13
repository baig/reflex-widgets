{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}
module Reflex.MDL.Snackbar where

import                   Prelude hiding (init)
import "base"            GHC.Generics (Generic)
import "base"            Control.Monad.IO.Class (liftIO)
import "text"            Data.Text (Text)
import "aeson"           Data.Aeson (ToJSON, FromJSON, toJSON)
import "reflex-dom"      Reflex.Dom
import "ghcjs-base"      GHCJS.Types
import "ghcjs-base"      GHCJS.Marshal (toJSVal)
import "ghcjs-dom-jsffi" GHCJS.DOM.Types (unElement)
import "ghcjs-dom-jsffi" GHCJS.DOM.Element (IsElement, toElement)
import                   Reflex.MDL.Common (materialize')

data Snackbar = Snackbar
              { message    :: !Text
              , timeout    :: !Int
              , actionText :: !Text
              } deriving (Show, Generic)

instance ToJSON Snackbar
instance FromJSON Snackbar

defaultSnackbar :: Text -> Snackbar
defaultSnackbar t = Snackbar t 2000 ""

data SnackbarOut = SnackbarOut
                   deriving (Show, Generic)

instance ToJSON SnackbarOut
instance FromJSON SnackbarOut

mdlSnackbar :: MonadWidget t m
            => Event t Snackbar
            -- ^ Snackbar data
            -> m (Event t SnackbarOut)
            -- ^ Snackbar action clicked
mdlSnackbar snackbarE = do
    -- snackbar element
    (element_, _) <- materialize' $ do
        elAttr' "div" ("class" =: "mdl-js-snackbar mdl-snackbar") $ do
            divClass "mdl-snackbar__text" blank
            elClass "button" "mdl-snackbar__action" blank

    -- output event + trigger
    (outE, triggerOutE) <- newTriggerEvent

    delayedSnackbarE <- delay 0.1 snackbarE
    -- handle input event
    performEvent_ $ ffor delayedSnackbarE $
        \snackbar -> liftIO $
            onSnackbar
                (_element_raw element_)
                triggerOutE
                snackbar

    return outE
    where
        onSnackbar :: (IsElement el)
                   => el
                   -- ^ Element
                   -> (SnackbarOut -> IO ())
                   -- ^ Trigger for out event
                   -> Snackbar
                   -- ^ snackbar data
                   -> IO ()
        onSnackbar element_ trigger snackbar = do
            init element_ snackbar
            trigger SnackbarOut


init :: (IsElement element)
     => element
     -- ^ Element to bind to
     -> Snackbar
     -- ^ Snackbar data
     -> IO ()
init element_ snackbar = do
    let js_element = unElement . toElement $ element_
    js_snackbar <- toJSVal . toJSON $ snackbar
    js_init js_element
            js_snackbar

foreign import javascript unsafe
    "(function() {                                                 \
    \    var snackbarContainer = $1;                               \
    \    snackbarContainer.MaterialSnackbar.showSnackbar($2);      \
    \})()"
    js_init :: JSVal
            -- ^ Element to use
            -> JSVal
            -- ^ Snackbar
            -> IO ()
