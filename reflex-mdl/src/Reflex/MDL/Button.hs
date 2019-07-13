{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Reflex.MDL.Button where

import "text"         Data.Text (Text, intercalate)
import "reflex-dom"   Reflex.Dom
import "base"         Data.Monoid ((<>))
import "base"         Control.Monad.IO.Class (liftIO)

import Reflex.MDL.Icon (Icon, mdlIcon)

mdlButton :: MonadWidget t m
          => Dynamic t Text
          -- ^ Button text
          -> m (Event t ())
mdlButton = flip mdlButtonWithClass []

mdlButtonWidgetUnraised :: MonadWidget t m
                        => m ()
                        -- ^ Inner widget
                        -> m (Event t ())
mdlButtonWidgetUnraised innerW  =
    mdlButtonWithWidgetExplicitClass innerW unraisedButtonClasses

mdlButtonWidgetUnraised' :: MonadWidget t m
                         => [Text]
                         -- ^ Additional classes
                         -> m ()
                         -- ^ Inner widget
                         -> m (Event t ())
mdlButtonWidgetUnraised' classes innerW =
    mdlButtonWithWidgetExplicitClass innerW $ unraisedButtonClasses ++ classes


mdlButtonWithClass :: MonadWidget t m
                   => Dynamic t Text
                   -- ^ Button text
                   -> [Text]
                   -- ^ Additional classes
                   -> m (Event t ())
mdlButtonWithClass buttonTextD classes = mdlButtonWithWidget (dynText buttonTextD) classes

mdlButtonWithWidget :: MonadWidget t m
                   => m ()
                   -- ^ Inner widget
                   -> [Text]
                   -- ^ Additional classes
                   -> m (Event t ())
mdlButtonWithWidget innerW classes = mdlButtonWithWidgetAndAttrs innerW classes mempty

mdlButtonWithExplicitClass :: MonadWidget t m
                           => Dynamic t Text
                           -- ^ Button text
                           -> [Text]
                           -- ^ Additional classes
                           -> m (Event t ())
mdlButtonWithExplicitClass buttonTextD classes =
    mdlButtonWithWidgetExplicitClass (dynText buttonTextD) classes

mdlButtonWithWidgetExplicitClass :: MonadWidget t m
                                 => m ()
                                 -- ^ Inner widget
                                 -> [Text]
                                 -- ^ Additional classes
                                 -> m (Event t ())
mdlButtonWithWidgetExplicitClass innerW classes =
    mdlButtonWithWidgetAndAttrsExplicit innerW classes mempty

mdlButtonWithWidgetAndAttrsExplicit :: MonadWidget t m
                                    => m ()
                                    -- ^ Inner widget
                                    -> [Text]
                                    -- ^ Additional classes
                                    -> AttributeMap
                                    -> m (Event t ())
mdlButtonWithWidgetAndAttrsExplicit innerW classes attrs = do
    mdlButtonWithWidgetAndDynAttrs innerW $ constDyn attrs'
    where
        attrs' = attrs <> ("class" =: classesString)
        classesString = intercalate " " $ classes

mdlButtonWithWidgetAndAttrsDExplicit :: MonadWidget t m
                                    => m ()
                                    -- ^ Inner widget
                                    -> [Text]
                                    -- ^ Additional classes
                                    -> Dynamic t AttributeMap
                                    -> m (Event t ())
mdlButtonWithWidgetAndAttrsDExplicit innerW classes attrsD = do
    mdlButtonWithWidgetAndDynAttrs innerW $ attrsD'
    where
        attrsD' = (("class" =: classesString) <>) <$> attrsD
        classesString = intercalate " " $ classes

mdlButtonWithWidgetAndDynAttrs :: MonadWidget t m
                               => m ()
                               -- ^ Inner widget
                               -> Dynamic t AttributeMap
                               -> m (Event t ())
mdlButtonWithWidgetAndDynAttrs innerW attrsD = do
    (el_, _) <- elDynAttr' "button" attrsD $ innerW
    let xE = domEvent Click el_
    return xE

defaultButtonClasses :: [Text]
defaultButtonClasses = "mdl-button--raised" : unraisedButtonClasses

unraisedButtonClasses :: [Text]
unraisedButtonClasses = [ "mdl-button"
                        , "mdl-js-button"
                        , "mdl-js-ripple-effect"
                        ]

mdlButtonWithWidgetAndAttrs :: MonadWidget t m
                   => m ()
                   -- ^ Inner widget
                   -> [Text]
                   -- ^ Additional classes
                   -> AttributeMap
                   -> m (Event t ())
mdlButtonWithWidgetAndAttrs innerW classes attrs =
    mdlButtonWithWidgetAndAttrsExplicit innerW
                                        (defaultButtonClasses ++ classes)
                                        attrs

mdlToggleButton :: MonadWidget t m
                => Icon
                -> Icon
                -> Event t Bool
                -- ^ control the toggle
                -> m (Dynamic t Bool)
mdlToggleButton firstIcon secondIcon outsideControlE = do
    (toggleE, toggleTrigger) <- newTriggerEvent
    toggleD <- toggle False toggleE
    let toggleIconD = boolToIcon <$> toggleD
    clickedE  <- mdlButtonWidgetUnraised (() <$ mdlIcon False toggleIconD)
    let outsideToggleE = attachPromptlyDynWithMaybe justIfNotEqual toggleD outsideControlE
        changeToggleE = leftmost [outsideToggleE, clickedE]
    performEvent_ $ ffor changeToggleE $ \_ -> liftIO $ toggleTrigger ()
    return toggleD
    where
        boolToIcon :: Bool -> Icon
        boolToIcon False = firstIcon
        boolToIcon True  = secondIcon

        justIfNotEqual :: Bool -> Bool -> Maybe ()
        justIfNotEqual x y = if x == y
            then Nothing
            else Just ()


mdlToggleButton' :: MonadWidget t m
                 => [Text]
                 -- ^ Additional classes
                 -> (Dynamic t Bool -> m ())
                 -> Event t Bool
                 -- ^ control the toggle
                 -> m (Dynamic t Bool)
mdlToggleButton' classes iconBuilder outsideControlE = do
    (toggleE, toggleTrigger) <- newTriggerEvent
    toggleD <- toggle False toggleE
    clickedE  <- mdlButtonWidgetUnraised' classes $ iconBuilder toggleD
    let outsideToggleE = attachPromptlyDynWithMaybe justIfNotEqual toggleD outsideControlE
        changeToggleE = leftmost [outsideToggleE, clickedE]
    performEvent_ $ ffor changeToggleE $ \_ -> liftIO $ toggleTrigger ()
    return toggleD
    where
        justIfNotEqual :: Bool -> Bool -> Maybe ()
        justIfNotEqual x y = if x == y
            then Nothing
            else Just ()
