{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Reflex.TensorFlowJS.TensorFlowJS where

import "base"            Control.Monad.IO.Class (liftIO)
import "base"            Data.IORef (IORef, newIORef, writeIORef, readIORef)
import "lens"            Control.Lens
import "aeson"           Data.Aeson (Value, ToJSON(..), FromJSON(..))
import "reflex-dom"      Reflex.Dom hiding (Value)
import "jsaddle"         Language.Javascript.JSaddle -- (JSVal) --  GHCJS.Types (JSVal)
import                   Reflex.TensorFlowJS.FFI

type Tensor2D = ([Double], [Double])

--
tfFit :: forall t m. (MonadWidget t m)
    => Event t (Tensor2D, Tensor2D)
    -- ^ Train event
    -> ((Tensor2D, Tensor2D) -> JSM (Model, Promise))
    -- ^ Callback
    -> m (Event t Model)
tfFit inputE callback = do
    -- output event + trigger
    (outE :: Event t Model, triggerOut) <- newTriggerEvent

    -- handle input event
    ctxRef <- askJSM
    performEvent_ $ ffor inputE $ \(xs, ys) -> flip runJSM ctxRef $ do
        (model, promise) <- callback (xs, ys)
        then_ promise $ do
            triggerOut model
        return ()

    return outE


--
tfPredict :: forall t m. (MonadWidget t m)
          => Event t (Model, Tensor2D)
          -- ^ Predict event
          -> ((Model, Tensor2D) -> JSM Tensor)
          -- ^ Callback
          -> m (Event t (Tensor, JSString))
tfPredict inputE callback = do
    -- output event + trigger
    (outE :: Event t (Tensor, JSString), triggerOut) <- newTriggerEvent

    -- handle input event
    ctxRef <- askJSM
    performEvent_ $ ffor inputE $ \(model, xs) -> flip runJSM ctxRef $ do
        result <- callback (model, xs)
        let result' = unTensor result
        array  <- valToJSON =<< result' ^. js0 ("dataSync" :: String)
        liftIO $ triggerOut (result, array)
        return ()

    return outE
