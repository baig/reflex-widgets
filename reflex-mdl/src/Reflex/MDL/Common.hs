{-# LANGUAGE PackageImports #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE MonoLocalBinds #-}
module Reflex.MDL.Common where

import "ghcjs-base"       GHCJS.Types (JSVal)
import "ghcjs-dom-jsffi"  GHCJS.DOM.Element (IsElement)
import "reflex-dom"       Reflex.Dom

foreign import javascript unsafe "componentHandler.upgradeElement($1);"
  materialInitJS :: JSVal -> IO ()

materialize :: (MonadWidget t m, IsElement el) => el -> m ()
materialize _ = return ()

materialize' :: MonadWidget t m
             => m (El t, a)
             -> m (El t, a)
materialize' widget = do
    (element_, x) <- widget
    materialize (_element_raw element_)
    return (element_, x)


