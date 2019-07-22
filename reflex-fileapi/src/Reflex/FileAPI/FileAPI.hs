{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.FileAPI.FileAPI where

import "base"       Control.Monad.IO.Class (liftIO)
import "stm"                  Control.Concurrent.STM (atomically)
import "stm-chans"            Control.Concurrent.STM.TBMQueue (newTBMQueueIO, readTBMQueue)
import "text"       Data.Text (Text)
import "jsaddle"    Language.Javascript.JSaddle
import              GHCJS.DOM.Element
import "reflex"     Reflex
import "reflex-dom" Reflex.Dom hiding (Value)
import              Reflex.FileAPI.FFI (readFileW)

filereader :: forall t m. (MonadWidget t m)
           => Int
           -- ^ Read chunk (step) size
           -> Event t ()
           -- ^ Input trigger
           -> m (Event t Text)
filereader step inputE = do
    -- HTML element
    (element', _) <- elAttr' "input"
                             ( "type" =: "file"
                            <> "name" =: "files[]"
                            <> "multiple" =: ""
                             )
                             blank

    -- output event + trigger
    (outE :: Event t Text, triggerOut) <- newTriggerEvent

    -- handle input event
    ctxRef <- askJSM
    performEvent_ $ ffor inputE $ \_ -> flip runJSM ctxRef $ onFile (_element_raw element')
                                                                    triggerOut

    return outE
    where
        onFile :: (IsElement el)
               => el
               -- ^ Element
               -> (Text -> IO ())
               -- ^ Trigger
               -> JSM ()
        onFile element' trigger = do
            q <- liftIO $ newTBMQueueIO (1024 * 1024)
            forkJSM $ do
                readFileW q
                         element'
                         0     -- start
                         step
                         Nothing
                         Nothing
                         Nothing
                         False
            _ <- liftIO $ atomically $ readTBMQueue q
            mText <- liftIO $ atomically $ readTBMQueue q
            case mText of
                Nothing -> return ()
                Just t  -> trigger t
            return ()
