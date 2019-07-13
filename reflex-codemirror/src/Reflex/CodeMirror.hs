{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
module Reflex.CodeMirror ( module Reflex.CodeMirror.Types
                         , module Reflex.CodeMirror.FFI
                         , codemirror
                         ) where
import "base"             Control.Monad.IO.Class (liftIO)
import "base"             Data.IORef (IORef, newIORef, writeIORef, readIORef)
import "text"             Data.Text (Text)
import "ghcjs-base"       GHCJS.Types (JSVal)
import "ghcjs-base"       GHCJS.Marshal (fromJSVal)
import "ghcjs-dom-jsffi"  GHCJS.DOM.Element (IsElement)
import "reflex-dom"       Reflex.Dom
import                    Reflex.CodeMirror.FFI
import                    Reflex.CodeMirror.Types hiding (configuration)

codemirror :: forall t m. (MonadWidget t m)
           => Configuration
           -> m (Event t Text)
codemirror configuration = do
    -- HTML element
    (element_, _) <- el' "textarea" blank

    -- local state
    (ref :: IORef (Maybe CodeMirrorRef)) <- liftIO $ newIORef Nothing

    -- input event
    (postBuildTaggedE :: Event t ()) <- getPostBuild
    let inputE = leftmost
               [ postBuildTaggedE
               {-, updated chartJsD-}
               ]

    -- output event + trigger
    (outE :: Event t Text, triggerOut) <- newTriggerEvent

    -- handle input event
    performEvent_ $ ffor inputE $ \_ -> liftIO $ handle (_element_raw element_)
                                                        ref
                                                        triggerOut
                                                        configuration
    return outE

    where
        handle :: (IsElement el)
               => el
               -- ^ Element
               -> IORef (Maybe CodeMirrorRef)
               -- ^ Local state
               -> (Text -> IO ())
               -- ^ Trigger for output event
               -> Configuration
               -- ^ Chart data
               -> IO ()
        handle element_ ref trigger configuration_ = do
            currentRef_ <- readIORef ref
            case currentRef_ of
                Nothing   -> onFirstTime element_ ref trigger configuration_
                Just ref_ -> onNextTime  ref_                 configuration_


        onFirstTime :: (IsElement el)
                    => el
                    -- ^ Element
                    -> IORef (Maybe CodeMirrorRef)
                    -- ^ Local state
                    -> (Text -> IO ())
                    -- ^ Trigger for output event
                    -> Configuration
                    -- ^ Chart data
                    -> IO ()
        onFirstTime element_ ref trigger configuration_ = do
            ref_ <- fromTextArea element_ configuration_
            writeIORef ref (Just ref_)

            onChange ref_
                     (onChangeCallback trigger)

            return ()


        onNextTime :: CodeMirrorRef
                   -- ^ Current value of local state
                   -> Configuration
                   -- ^ Chart data
                   -> IO ()
        onNextTime _ _ = do
            return ()


        onChangeCallback :: (Text -> IO ())
                         -> JSVal
                         -> IO ()
        onChangeCallback trigger jsval_ = do
            Just (json_ :: Text) <- fromJSVal jsval_
            {-let Just (event :: ChartJsSelection) = Aeson.decodeStrict . encodeUtf8 $ json_-}
            let event = json_
            --print event
            trigger event
            return ()
