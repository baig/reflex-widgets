{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
module Reflex.Utils.Arbitrary where

import "base"       Control.Monad.IO.Class (liftIO)
import "reflex-dom" Reflex.Dom
import "QuickCheck" Test.QuickCheck.Gen hiding (sample)
import "QuickCheck" Test.QuickCheck.Arbitrary

randomsW :: forall a t m. (MonadWidget t m, Arbitrary a)
         => Event t ()
         -- ^ trigger
         -> m (Event t [a])
randomsW triggerE = do
    (outE, triggerOut) <- newTriggerEvent
    performEvent_ $ ffor triggerE $ \_ -> liftIO $ do
        randoms_ <- sample' $ resize 2 arbitrary
        triggerOut randoms_
        return ()
    return outE
