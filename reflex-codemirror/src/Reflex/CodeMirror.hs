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
import "jsaddle"          Language.Javascript.JSaddle -- (JSVal) --  GHCJS.Types (JSVal)
import "reflex-dom"       Reflex.Dom
import                    GHCJS.DOM.Element hiding (scrollIntoView) -- (IsElement)
import                    Reflex.CodeMirror.FFI
import                    Reflex.CodeMirror.Types hiding (configuration)

codemirror :: forall t m. (MonadWidget t m)
           => Configuration
           -> Event t (Maybe LineChar)
           -> m (Event t Text)
codemirror configuration scrollToE = do
    -- HTML element
    (element_, _) <- el' "textarea" blank

    -- local state
    (ref :: IORef (Maybe CodeMirrorRef)) <- liftIO $ newIORef Nothing

    -- input event
    (postBuildTaggedE :: Event t ()) <- getPostBuild
    let inputE = leftmost
               [ Nothing <$ postBuildTaggedE
               , scrollToE
               ]

    -- output event + trigger
    (outE :: Event t Text, triggerOut) <- newTriggerEvent

    -- handle input event
    ctxRef <- askJSM
    performEvent_ $ ffor inputE $ \mScrollTo -> flip runJSM ctxRef $ handle (_element_raw element_)
                                                             ref
                                                             triggerOut
                                                             configuration
                                                             mScrollTo
    return outE

    where
        handle :: (IsElement el)
               => el
               -- ^ Element
               -> IORef (Maybe CodeMirrorRef)
               -- ^ Local state
               -> (Text -> JSM ())
               -- ^ Trigger for output event
               -> Configuration
               -- ^ Chart data
               -> (Maybe LineChar)
               -- ^ Scroll to
               -> JSM ()
        handle element_ ref trigger configuration_ mScrollTo = do
            currentRef_ <- liftIO $ readIORef ref
            case currentRef_ of
                Nothing   -> onFirstTime element_ ref trigger configuration_ mScrollTo
                Just ref_ -> onNextTime  ref_                 configuration_ mScrollTo


        onFirstTime :: (IsElement el)
                    => el
                    -- ^ Element
                    -> IORef (Maybe CodeMirrorRef)
                    -- ^ Local state
                    -> (Text -> JSM ())
                    -- ^ Trigger for output event
                    -> Configuration
                    -- ^ Chart data
                    -> (Maybe LineChar)
                    -- ^ Scroll To
                    -> JSM ()
        onFirstTime element_ ref trigger configuration_ mScrollTo = do
            ref_ <- fromTextArea element_ configuration_
            liftIO $ writeIORef ref (Just ref_)
            registerOnChange ref_
                             (onChangeCallback trigger)
            case mScrollTo of
                Nothing       -> return ()
                Just scrollTo_ -> scrollIntoView ref_ scrollTo_ 200

            return ()


        onNextTime :: CodeMirrorRef
                   -- ^ Current value of local state
                   -> Configuration
                   -- ^ Chart data
                    -> (Maybe LineChar)
                    -- ^ Scroll To
                   -> JSM ()
        onNextTime ref_ _ mScrollTo = do
            case mScrollTo of
                Nothing        -> return ()
                Just scrollTo_ -> scrollIntoView ref_ scrollTo_ 200


        onChangeCallback :: (Text -> JSM ())
                         -> Text
                         -> JSM ()
        onChangeCallback trigger t = do
            liftIO $ trigger t
            return ()
