{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
module Reflex.Utils.Display where

import           "text"         Data.Text (Text)
import qualified "containers"   Data.Map as Map
import           "reflex-dom"   Reflex.Dom

divDynDisplayIf :: MonadWidget t m
                => Dynamic t Bool
                -- ^ display div when true
                -> m a
                -- ^ widget
                -> m a
divDynDisplayIf = elDynDisplayIf "div"

elDynDisplayIf :: MonadWidget t m
               => Text
               -- ^ type of element
               -> Dynamic t Bool
               -- ^ display div when true
               -> m a
               -- ^ widget
               -> m a
elDynDisplayIf elType = elDynAttrDisplayIf elType (constDyn mempty)

elDynAttrDisplayIf :: MonadWidget t m
                   => Text
                   -- ^ type of element
                   -> Dynamic t AttributeMap
                   -- ^ attributes
                   -> Dynamic t Bool
                   -- ^ display div when true
                   -> m a
                   -- ^ widget
                   -> m a
elDynAttrDisplayIf elType attrsD boolD = elDynAttr elType attrsD'
    where
        attrsD' = (<>) <$> attrsD <*> (toAttrs <$> boolD)

        toAttrs :: Bool -> Map.Map Text Text
        toAttrs True  = mempty
        toAttrs False = "style" =: "display: none"

sizedDivDynDisplayIf :: MonadWidget t m
                     => Dynamic t Bool
                     -- ^ display div when true
                     -> m a
                     -- ^ widget
                     -> m a
sizedDivDynDisplayIf boolD = elDynAttr "div" attrsD
    where
        attrsD = toAttrs <$> boolD

        toAttrs :: Bool -> Map.Map Text Text
        toAttrs True  = mempty
        toAttrs False = "style" =: "visibility: hidden"
