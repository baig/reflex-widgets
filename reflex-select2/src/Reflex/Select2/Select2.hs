{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module Reflex.Select2.Select2 ( select2
                              ) where

import           "base"            Control.Monad (void)
import           "base"            Control.Monad.IO.Class (liftIO)
import           "base"            Data.IORef (IORef, newIORef, writeIORef, readIORef)
import           "text"            Data.Text (Text)
import           "jsaddle"         Language.Javascript.JSaddle -- (JSVal) --  GHCJS.Types (JSVal)
import           "reflex-dom"      Reflex.Dom
import                             GHCJS.DOM.Element -- (IsElement)
import                             Reflex.Select2.Types hiding (select2)
import                             Reflex.Select2.FFI


select2 :: forall t m. (MonadWidget t m)
        => Select2
        -- ^ Select2
        -> Dynamic t [Text]
        -- ^ Texts
        -> m (Dynamic t [Text])
        -- ^ Current data
select2 select2' textsD = do
    postBuildE <- getPostBuild

    -- select2 element
    (el_, _) <- elAttr' "select" mempty blank

    -- local state
    (select2Ref :: IORef (Maybe Select2Ref)) <- liftIO $ newIORef Nothing

    -- output event + trigger
    (outE, triggerOutE) <- newTriggerEvent

    let inputE = leftmost
               [ tagPromptlyDyn textsD postBuildE
               , updated textsD
               ]

    -- handle input event
    ctxRef <- askJSM
    performEvent_ $ ffor inputE $ \texts -> flip runJSM ctxRef $ onInput (_element_raw el_)
                                                                         select2Ref
                                                                         triggerOutE
                                                                         texts
    outD <- holdDyn [] outE
    return $ outD

    where
        onInput :: (IsElement el)
                => el
                -- ^ Element
                -> IORef (Maybe Select2Ref)
                -- ^ Select2 ref
                -> ([Text] -> JSM ())
                -- ^ Trigger for output event
                -> [Text]
                -- ^ data
                -> JSM ()
        onInput element_
                select2Ref
                trigger
                texts = do
            currentRef_ <- liftIO $ readIORef select2Ref
            case currentRef_ of
                Nothing -> onFirstTime element_ select2Ref trigger texts
                Just ref  -> void $ onNextTime  element_ select2Ref ref texts

        onFirstTime :: (IsElement el)
                    => el
                    -- ^ Element
                    -> IORef (Maybe Select2Ref)
                    -- ^ Local state
                    -> ([Text] -> JSM ())
                    -- ^ Trigger for output event
                    -> [Text]
                    -- ^ data
                    -> JSM ()
        onFirstTime element_ select2Ref _ texts = do
            ref <- newSelect2 element_ select2'
            liftIO $ writeIORef select2Ref (Just ref)
            _ <- update ref texts
            return ()


        onNextTime :: (IsElement el)
                   => el
                   -- ^ Element
                   -> IORef (Maybe Select2Ref)
                   -- ^ Local state
                   -> Select2Ref
                   -- ^ ref
                   -> [Text]
                   -- ^ data
                   -> JSM Select2Ref
        onNextTime _ _ ref texts = do
            _ <- update ref texts
            return ref
