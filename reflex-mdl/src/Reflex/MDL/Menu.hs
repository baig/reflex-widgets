{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module Reflex.MDL.Menu ( mdlMenu
                       , mdlMenuDynButton
                       , MdlMenuConfig(..)
                       , rightAlignBottomDropClasses
                       , leftAlignBottomDropClasses
                       )
                       where

import "data-default"         Data.Default     (Default, def)
import "reflex-dom"           Reflex.Dom
import "base"                 Data.Monoid ((<>))
import                        Reflex.MDL.Button
import qualified "text"       Data.Text as T
import qualified "containers" Data.Map as Map
--import "base"                 Control.Monad
import "base"                 Data.Maybe
import "base"                 Control.Monad.IO.Class (liftIO)


---------------------- menu config ----------------------------------------

data MdlMenuConfig a t m = MdlMenuConfig
                         { mdlMenuConfig_classes            :: !(Dynamic t [T.Text])
                         , mdlMenuConfig_id                 :: !(Dynamic t T.Text)
                         , mdlMenuConfig_opts               :: ![a]
                         , mdlMenuConfig_optWidget          :: !(a -> m ())
                         , mdlMenuConfig_extraOptAttrs      :: !(a -> Maybe AttributeMap)
                         , mdlMenuConfig_buttonExtraClasses :: ![T.Text]
                         , mdlMenuConfig_buttonWidget       :: !(m ())
                         }


instance (MonadWidget t m, Enum a) => Default (MdlMenuConfig a t m) where
    def = MdlMenuConfig
        { mdlMenuConfig_classes              = constDyn rightAlignBottomDropClasses
        , mdlMenuConfig_id                   = constDyn "default_menu_id"
        , mdlMenuConfig_opts                 = [toEnum 0 ..]
        , mdlMenuConfig_optWidget            = const blank
        , mdlMenuConfig_extraOptAttrs        = const Nothing
        , mdlMenuConfig_buttonExtraClasses   = []
        , mdlMenuConfig_buttonWidget         = blank
        }

----------------------- constants --------------------------


rightAlignBottomDropClasses :: [T.Text]
rightAlignBottomDropClasses = [ "mdl-menu"
                              , "mdl-menu--bottom-right"
                              , "mdl-js-menu"
                              , "mdl-js-ripple-effect"
                              ]

leftAlignBottomDropClasses :: [T.Text]
leftAlignBottomDropClasses = [ "mdl-menu"
                             , "mdl-menu--bottom-left"
                             , "mdl-js-menu"
                             , "mdl-js-ripple-effect"
                             ]


----------------------- interface --------------------------


mdlMenuDynButton :: (MonadWidget t m, Eq a,  Enum a)
                 => MdlMenuConfig a t m ---(Dynamic t a -> m (Event t a))
                 -- ^ menu waiting for trigger event
                 -> (Dynamic t a -> m ())
                 -- ^ trigger function
                 -> a
                 -- ^ initial value
                 -> Event t a
                 -- ^ control the toggle
                 -> m (Dynamic t a)
mdlMenuDynButton menuConfig triggerFunction initVal outsideControlE  = do
    (toggleE, toggleTrigger) <- newTriggerEvent
    toggleD <- holdDyn initVal toggleE
    clickedE  <- mdlMenu $ menuConfig { mdlMenuConfig_buttonWidget = triggerFunction toggleD}
    let outsideToggleE = attachPromptlyDynWithMaybe justIfNotEqualReturnLast
                                                    toggleD
                                                    outsideControlE
        changeToggleE = leftmost [outsideToggleE, clickedE]
    performEvent_ $ ffor changeToggleE $ \ls -> liftIO $ toggleTrigger ls
    return toggleD
    where
        justIfNotEqualReturnLast :: Eq a => a -> a -> Maybe a
        justIfNotEqualReturnLast x y = if x == y
                                       then Nothing
                                       else Just y


mdlMenu :: (MonadWidget t m,  Enum a)
        => MdlMenuConfig a t m
        -> m (Event t a)
mdlMenu (MdlMenuConfig menuClassesD
                       menuIdD
                       opts
                       optWidget
                       extraOptAttrs
                       buttonExtraClasses
                       buttonWidget
        ) = do
  let buttonAttrsD = buttonAttrs <$> menuIdD
      menuAttrsD = menuAttrs <$> menuClassesD <*> menuIdD
      buttonClasses = [ "mdl-button"
                      , "mdl-js-ripple-effect" -- "mdl-button--icon"
                      , "mdl-js-button"
                      ] ++ buttonExtraClasses
  _ <- mdlButtonWithWidgetAndAttrsDExplicit buttonWidget buttonClasses buttonAttrsD
  elDynAttr "ul" menuAttrsD $ do
    opts' <- mapM (menuOptionAction optWidget extraOptAttrs) opts
    return $ leftmost opts'
  where
    menuAttrs classes menuId = "class" =: (T.unwords $ classes)
                               <> "for" =: menuId
    buttonAttrs menuId = "id" =: menuId



menuOptionAction :: forall t m a. (MonadWidget t m)
                 => (a -> m ())
                 -> (a -> Maybe AttributeMap)
                 -> a
                 -> m (Event t a)
menuOptionAction iconBuilder extraAttrsPerOpt menuOpt = do
  let extraAttrs = extraAttrsPerOpt menuOpt
  ev <- menuOptionClick extraAttrs $ --do
    iconBuilder menuOpt
  return (menuOpt <$ ev)

-- this is the click event
menuOptionClick :: MonadWidget t m => Maybe AttributeMap -> m () -> m (Event t ())
menuOptionClick extraAttrs widg = do
   (e,_) <- elAttr' "li" optionAttrs widg
   return $ domEvent Click e
       where
         optionAttrs = Map.unionWith (\a b -> a <> " " <> b)
                                     ("class" =: "mdl-menu__item")
                                     (fromMaybe Map.empty extraAttrs)
