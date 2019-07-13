{-# LANGUAGE PackageImports #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
module Reflex.MDL.Tooltip where

import                        Prelude hiding (init)
import           "base"       Data.Monoid ((<>))
import           "text"       Data.Text (Text)
import           "reflex-dom" Reflex.Dom
import                        Reflex.MDL.Common

mdlTooltip :: forall a t m. (MonadWidget t m)
           => Dynamic t Text
           -- ^ HTML ID
           -> Dynamic t Text
           -- ^ Tooltip text
           -> m a
           -- ^ Wrapped widget
           -> m a
mdlTooltip htmlIdD tooltipD widget = do
    -- let attrsD = ("id" =:) <$> htmlId :: Dynamic t (Map Text Text)
    (_, result) <- elDynAttr' "div" attrsD widget
    _ <- materialize' $  elDynAttr' "span" tooltipAttrsD $ dynText tooltipD

    return result
    where
        attrsD = ("id" =:) <$> htmlIdD
        tooltipAttrsD = (\x -> "class" =: "mdl-tooltip" <> "for" =: x) <$> htmlIdD
