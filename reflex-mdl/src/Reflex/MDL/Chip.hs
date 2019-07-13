{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
module Reflex.MDL.Chip where

import "text"         Data.Text (Text)
import "reflex-dom"   Reflex.Dom

mdlChip :: MonadWidget t m
        => Dynamic t Text
        -- ^ Chip text
        -> m ()
mdlChip chipText= do
    elClass "span" "mdl-chip" $ do
        elClass "span" "mdl-chip__text" $ do
            dynText chipText
