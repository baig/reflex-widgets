{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}
module Reflex.MDL.Drawer where

import              Prelude hiding (init)
import "base"       Control.Monad.IO.Class (liftIO)
import "reflex-dom" Reflex.Dom
import              Reflex.MDL.Common (materialize')

mdlDrawer :: MonadWidget t m
          => m (Event t a)
          -- ^ Widget
          -> m (Event t a)
mdlDrawer widget = do
    -- drawer element
    (_, selectionE) <- materialize' $ do
        elAttr' "div" ("class" =: "mdl-layout__drawer") $ do
            widget

    -- toggle drawer on selection event
    performEvent_ $ ffor selectionE $ \_ -> liftIO js_toggle
    
    return selectionE


foreign import javascript unsafe
    "(function() {                                                           \
    \    var layout = document.querySelector('.mdl-layout');                 \
    \    var obfuscator = document.querySelector('.mdl-layout__obfuscator'); \
    \    if (obfuscator.classList.contains('is-visible')) {                  \
    \       layout.MaterialLayout.toggleDrawer();                            \
    \    }                                                                   \
    \})()"
    js_toggle :: IO ()
