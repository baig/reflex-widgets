{-# LANGUAGE PackageImports #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
module Reflex.MDL.Sidebar where

import                        Prelude hiding (init)
import           "containers" Data.Map (fromList)
import           "text"       Data.Text (Text)
import           "reflex-dom" Reflex.Dom
import                        Reflex.MDL.Drawer
import                        Reflex.MDL.Icon
import                        Reflex.MDL.Grid


mdlSidebar :: forall a t m. (Eq a, MonadWidget t m)
           => [a]
           -- ^ Options
           -> (a -> Text)
           -- ^ Text per option
           -> (a -> Icon)
           -- ^ Icon per option
           -> Dynamic t a
           -- ^ Current selection
           -> m ()
           -- ^ Title Widget
           -> m (Event t a)
           -- ^ Output: new selection
mdlSidebar options
           toText
           toIcon
           optionD
           titleWidget = do
    newSelectionE <- mdlDrawer $ do
        mdlGrid $ do
            elClass "header" mempty $ do
               titleWidget
        mdlGrid $ do
            elClass "nav" "mdl-navigation" $ do
                leftmost <$> mapM (optionWidget toText toIcon optionD) options
    return newSelectionE


optionWidget :: forall a t m. (Eq a, MonadWidget t m)
             => (a -> Text)
             -- ^ Text per option
             -> (a -> Icon)
             -- ^ Icon per option
             -> Dynamic t a
             -- ^ current selected option
             -> a
             -- ^ Widget to this option
             -> m (Event t a)
optionWidget toText
             toIcon
             currentOptionD
             option = do
    (el_, _) <- elDynAttr' "span" attrsD $ do
        -- icon for the option
        _ <- mdlIcon True $ constDyn $ toIcon option

        -- text for the option
        text $ toText option

    -- event for selecting this option
    let selectionE = option <$ domEvent Click el_
    return selectionE

    where
        attrs currentOption
            = fromList
            [ ("href", "")
            , ("class", if  currentOption == option
                        then "mdl-navigation__link mdl-color--primary"
                        else "mdl-navigation__link"
              )
            ]

        attrsD = attrs <$> currentOptionD
