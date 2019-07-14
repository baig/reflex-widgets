{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.Utils.Load where

import "base"       Data.Monoid()
import "text"       Data.Text as T
import "reflex-dom" Reflex.Dom
import "base"       Data.Semigroup()


instance Semigroup Bool where
    b1 <> b2 = b1 && b2

instance Monoid Bool where
    b1 `mappend` b2 = b1 && b2
    mempty = True


whenLoaded :: forall t m. MonadWidget t m
           => [Dynamic t Bool]
           -- ^ Wait for these to load
           -> m ()
           -- ^ Loading widget
           -> m ()
           -- ^ Loaded widget
           -> m (Dynamic t Bool)
whenLoaded loadedDs
           loadingWidget
           loadedWidget = do
    allLoadedD <- return . mconcat $ loadedDs
    let allLoadedE = ffilter id .  updated $ allLoadedD
    delayedLoadedE <- delay 0.5 allLoadedE
    changeD <- widgetHold loadingWidget $ ffor delayedLoadedE $ \_ -> do
        loadedWidget
    changeE_ <- headE $ updated changeD
    let changeE = True <$ changeE_
    holdDyn False changeE


script :: forall t m. MonadWidget t m
       => Text
       -- ^ URI
       -> m (Dynamic t Bool)
script uri = do
    (element_ , _) <- elAttr' "script"
                              ("src" =: uri)
                              blank
    let loadedE = (True <$ domEvent Load element_)
    holdDyn False loadedE


css :: forall t m. MonadWidget t m
    => Text
    -- ^ URI
    -> m (Dynamic t Bool)
css uri = do
    (element_, _) <- elAttr' "link"
                             ( "rel"  =: "stylesheet"
                            <> "type" =: "text/css"
                            <> "href" =: uri
                             )
                             blank
    let loadedE = (True <$ domEvent Load element_)
    holdDyn False loadedE

