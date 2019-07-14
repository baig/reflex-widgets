{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Reflex.JsonEditor.JsonEditor ( jsoneditor
                                    ) where

import "base"            Control.Monad.IO.Class (liftIO)
import "base"            Data.IORef (IORef, newIORef, writeIORef, readIORef)
import "aeson"           Data.Aeson (ToJSON(..), FromJSON(..))
import "reflex-dom"      Reflex.Dom
import "ghcjs-dom-jsffi" GHCJS.DOM.Element (IsElement)
import                   Reflex.JsonEditor.FFI


--
jsoneditor :: forall a t m. (ToJSON a, FromJSON a, Eq a, MonadWidget t m)
           => Dynamic t a
           -- ^ value
           -> m (Dynamic t a)
           -- ^ out - changed value
jsoneditor jsonableD = do
    newJsonableE_ <- jsoneditor_ jsonableD
    let newJsonableE = fmapMaybe id newJsonableE_ -- fires only when (Just _)
    initial <- sample . current $ jsonableD
    newJsonableD <- holdDyn initial newJsonableE
    return $ newJsonableD


--
jsoneditor_ :: forall a t m. (ToJSON a, FromJSON a, Eq a, MonadWidget t m)
            => Dynamic t a
            -- ^ value
            -> m (Event t (Maybe a))
            -- ^ out - changed value
jsoneditor_ jsonableD = do
--    postBuildE <- getPostBuild

    -- jsoneditor element
    (el_, _) <- el' "div" blank

    -- local state
    (jsonEditorRef :: IORef (Maybe JsonEditorRef)) <- liftIO $ newIORef Nothing

    -- output event + trigger
    (outE, triggerOutE) <- newTriggerEvent

    let inputE = leftmost
               [ updated jsonableD
               ]

    -- handle input event
    performEvent_ $ ffor inputE $ \jsonable -> liftIO $ onInput (_element_raw el_)
                                                                jsonEditorRef
                                                                triggerOutE
                                                                jsonable

    return $ outE

    where
        onInput :: (IsElement el)
                => el
                -- ^ Element
                -> IORef (Maybe JsonEditorRef)
                -- ^ Local state
                -> (Maybe a -> IO ())
                -- ^ Trigger for output event
                -> a
                -- ^ data
                -> IO ()
        onInput element_ jsonEditorRef trigger jsonable = do
            currentRef_ <- readIORef jsonEditorRef
            case currentRef_ of
                Nothing  -> onFirstTime element_ jsonEditorRef trigger jsonable
                Just ref -> onNextTime  ref                    trigger jsonable

        onFirstTime :: (IsElement el)
                    => el
                    -- ^ Element
                    -> IORef (Maybe JsonEditorRef)
                    -- ^ Local state
                    -> (Maybe a -> IO ())
                    -- ^ Trigger for output event
                    -> a
                    -- ^ json data
                    -> IO ()
        onFirstTime element_ jsonEditorRef trigger jsonable_ = do
            ref <- newJsonEditor element_ def
            writeIORef jsonEditorRef (Just ref)
            set ref jsonable_
            trigger (Just jsonable_)
            return ()


        onNextTime :: JsonEditorRef
                   -- ^ Current value of local state
                    -> (Maybe a -> IO ())
                    -- ^ Trigger for output event
                   -> a
                   -- ^ json data
                   -> IO ()
        onNextTime jsonEditorRef trigger jsonable_ = do
            set jsonEditorRef jsonable_
            trigger (Just jsonable_)
            return ()
