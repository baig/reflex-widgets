{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
module Reflex.MDL.Layout where

import "base"         Data.Monoid ((<>))
import "text"         Data.Text (Text)
import "reflex-dom"   Reflex.Dom
import                Reflex.MDL.Common (materialize')

mdlLayout :: MonadWidget t m
          => Bool
          -- ^ Fixed header
          -> Bool
          -- ^ Fixed drawer
          -> m (Dynamic t a)
          -- ^ Widget
          -> m (Dynamic t a)
mdlLayout fixedHeader fixedDrawer widget = do
    (_, xD) <- materialize' $ do
        elAttr' "div" ("class" =: layoutClass) $ do
            widget
    return xD
    where
        layoutClass :: Text
        layoutClass = "mdl-layout"
            <> " " <> "mdl-js-layout"
            <> (if fixedHeader then " mdl-layout--fixed-header" else "")
            <> (if fixedDrawer then " mdl-layout--fixed-drawer" else "")


spacer :: MonadWidget t1 m => m ()
spacer =  divClass "mdl-layout-spacer" $ return ()
