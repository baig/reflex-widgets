{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}
module Reflex.MDL.Table where

import           "base"         Data.Monoid ((<>))
import qualified "text"         Data.Text as T
import           "reflex-dom"   Reflex.Dom
import                          Reflex.MDL.Common (materialize')

mdlTable :: MonadWidget t m
          => m a
          -- ^ Widget
          -> m a
mdlTable = mdlTableClass []


mdlTableClass :: MonadWidget t m
              => [T.Text]
              -- ^ Class
              -> m a
              -- ^ Widget
              -> m a
mdlTableClass cls widget = do
    -- table element
    (_, result) <- materialize' $ do
        elAttr' "table" ( "class" =: clss
                       <> "width" =: "100%"
                       <> "cellspacing" =: "0"
                        ) $ do
            widget
    return result
    where
        clss = T.intercalate " "
                             $ "mdl-data-table mdl-js-data-table mdl-shadow--2dp":cls

