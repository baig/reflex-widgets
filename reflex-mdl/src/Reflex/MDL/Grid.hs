{-# LANGUAGE PackageImports #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
module Reflex.MDL.Grid where

import                        Prelude hiding (init, div)
import           "base"       Data.Monoid ((<>))
import qualified "containers" Data.Map as Map
import qualified "text"       Data.Text as T
import           "reflex-dom" Reflex.Dom
{-import                        Reflex.MDL.Common-}

-- ↓Closed form functions, e.g. `mdlCell 4 $ text "The name is John doe"`

mdlGrid :: DomBuilder t m
        => m a
        -> m a
mdlGrid = div mdlGrid'

mdlGridId :: DomBuilder t m
          => T.Text
          -> m a
          -> m a
mdlGridId = idOnly mdlGrid'

mdlGridCl :: DomBuilder t m
          => T.Text
          -> m a
          -> m a
mdlGridCl = classOnly mdlGrid'

mdlGridClId :: DomBuilder t m
               => T.Text -> T.Text -> m a -> m a
mdlGridClId = classAndId mdlGrid'

--

mdlCell :: forall t m a. MonadWidget t m
        => Int
        -> m a
        -> m a
mdlCell cols = div (mdlCellCols' cols)

mdlCellId :: DomBuilder t m
          => Int
          -> T.Text
          -> m a
          -> m a
mdlCellId cols = idOnly (mdlCellCols' cols)

mdlCellCl :: DomBuilder t m
          => Int
          -> T.Text
          -> m a
          -> m a
mdlCellCl cols = classOnly (mdlCellCols' cols)

mdlCellClId :: DomBuilder t m
            => Int
            -> T.Text
            -> T.Text
            -> m a
            -> m a
mdlCellClId cols = classAndId (mdlCellCols' cols)

mdlLayoutSpacer :: DomBuilder t m
                => m ()
mdlLayoutSpacer = div mdlLayoutSpacer' blank

--

idOnly :: DomBuilder t m
       => (AttributeMap -> AttributeMap) -> T.Text -> m a -> m a
idOnly f identification = div (f . setId identification)

classOnly :: DomBuilder t m
          => (AttributeMap -> AttributeMap) -> T.Text -> m a -> m a
classOnly f identification = div (f . addClass identification)

classAndId :: DomBuilder t m
           => (AttributeMap -> AttributeMap) -> T.Text -> T.Text -> m a -> m a
classAndId f identification classText = div (f . setId identification . addClass classText)

--

div :: DomBuilder t m
    => (AttributeMap -> AttributeMap)
    -> m a
    -> m a
div attrF = divAttr (attrF Map.empty)

divDyn :: (DomBuilder t m, PostBuild t m)
       => Dynamic t (AttributeMap -> AttributeMap)
       -> m a
       -> m a
divDyn attrFD = divDynAttr $ do
    attrF <- attrFD
    return $ attrF Map.empty

-- ↓MDL-specific modifiers

mdlGrid' :: AttributeMap -> AttributeMap
mdlGrid' = addClass "mdl-grid"

mdlCell' :: AttributeMap -> AttributeMap
mdlCell' = addClass "mdl-cell"

mdlLayoutSpacer' :: AttributeMap -> AttributeMap
mdlLayoutSpacer' = addClass "mdl-layout-spacer"

mdlCellCols' :: Int -> AttributeMap -> AttributeMap
mdlCellCols' cols = addClass "mdl-cell"
                  . addClass ("mdl-cell--" <> (T.pack $ show cols) <> "-col")

-- ↓Generic class and ID modifiers

addClass :: T.Text -> AttributeMap -> AttributeMap
addClass classText = Map.insertWith (\existing new -> T.intercalate " " [existing, new])
                                    "class"
                                    classText

setId :: T.Text -> AttributeMap -> AttributeMap
setId idText = Map.insert "id" idText

--

divAttr :: DomBuilder t m
        => AttributeMap -> m a -> m a
divAttr attrs = elAttr "div" attrs

divDynAttr :: (DomBuilder t m,  PostBuild t m)
           => Dynamic t AttributeMap -> m a -> m a
divDynAttr attrsD = elDynAttr "div" attrsD

