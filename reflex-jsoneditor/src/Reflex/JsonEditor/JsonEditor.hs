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
                                    , JsonEditorOptions
                                    ) where

import "base"            Control.Monad.IO.Class (liftIO)
import "base"            Data.IORef (IORef, newIORef, writeIORef, readIORef)
import "text"            Data.Text (unpack)
import "bytestring"      Data.ByteString.Lazy.Char8 (pack)
import "aeson"           Data.Aeson (ToJSON(..), FromJSON(..), decode)
import "reflex-dom"      Reflex.Dom hiding (Value(..))
import "ghcjs-dom-jsffi" GHCJS.DOM.Element (IsElement)
import "jsaddle"         Language.Javascript.JSaddle
import                   Reflex.JsonEditor.FFI
import                   Reflex.JsonEditor.Types (JsonEditorOptions, JsonEditorSelection, JsonEditorEvent(..))


data JsonEditor a
    = JsonEditor
    { _jsonEditor_trigger       :: (ToJSON a, FromJSON a) => Maybe a -> IO ()
    , _jsonEditor_triggerEvent  :: JsonEditorEvent -> IO ()
    }

instance (ToJSON a, FromJSON a) => JsonEditorHandlers (JsonEditor a) where
    onChangeJSON self jsval' = do
        json' <- strToText <$> valToJSON jsval'
        let json = decode . pack . unpack $ json'
        liftIO $ (_jsonEditor_trigger self) json

    onEvent self node'' _ = do
        node' <- strToText <$> valToJSON node''
        let (mNode :: Maybe JsonEditorEvent) = decode . pack . unpack $ node'
        --event' <- strToText <$> valToJSON event''
        --let event = decode . pack . unpack $ event'
        case mNode of
            Nothing   -> return ()
            Just node -> liftIO $ (_jsonEditor_triggerEvent self node)

    onSelectionChange _ args = do
        let json = head args
        json' <- strToText <$> valToJSON json
        liftIO $ print json'
        let (mSelection :: Maybe JsonEditorSelection) = decode . pack . unpack $ json'
        case mSelection of
            Nothing -> return ()
            Just selection -> do
                --liftIO $ (_jsonEditor_trigger self) selection
                liftIO $ print selection

--
jsoneditor :: forall a t m. (ToJSON a, FromJSON a, Eq a, MonadWidget t m)
           => JsonEditorOptions
           -- ^ Options
           -> Dynamic t a
           -- ^ value
           -> m ( Dynamic t a
                , Event t (Maybe JsonEditorEvent)
                )
           -- ^ out - changed value
jsoneditor options jsonableD = do
    (newJsonableE_, selectE) <- jsoneditor_ options jsonableD
    let newJsonableE = fmapMaybe id newJsonableE_ -- fires only when (Just _)
    initial <- sample . current $ jsonableD
    newJsonableD <- holdDyn initial newJsonableE
    return $ (newJsonableD, selectE)


--
jsoneditor_ :: forall a t m. (ToJSON a, FromJSON a, Eq a, MonadWidget t m)
            => JsonEditorOptions
            -- ^ Options
            -> Dynamic t a
            -- ^ value
            -> m ( Event t (Maybe a)
                 , Event t (Maybe JsonEditorEvent)
                 )
            -- ^ out - changed value
jsoneditor_ options jsonableD = do
--    postBuildE <- getPostBuild

    -- jsoneditor element
    (el_, _) <- el' "div" blank

    -- local state
    (jsonEditorRef :: IORef (Maybe JsonEditorRef)) <- liftIO $ newIORef Nothing

    -- output event + trigger
    (outE, triggerOut) <- newTriggerEvent

    -- selection event + trigger
    (eventE', triggerEvent) <- newTriggerEvent
    let eventE = leftmost
               [ Nothing <$ outE
               , Just   <$> eventE'
               ]


    let inputE = leftmost
               [ updated jsonableD
               ]

    -- handle input event
    performEvent_ $ ffor inputE $ \jsonable -> liftIO $ onInput (_element_raw el_)
                                                                jsonEditorRef
                                                                triggerOut
                                                                triggerEvent
                                                                jsonable

    return $ (outE, eventE)

    where
        onInput :: (IsElement el)
                => el
                -- ^ Element
                -> IORef (Maybe JsonEditorRef)
                -- ^ Local state
                -> (Maybe a -> IO ())
                -- ^ Trigger for output event
                -> (JsonEditorEvent -> IO ())
                -- ^ Trigger for selection
                -> a
                -- ^ data
                -> IO ()
        onInput element_ jsonEditorRef trigger triggerEvent jsonable = do
            currentRef_ <- readIORef jsonEditorRef
            case currentRef_ of
                Nothing  -> onFirstTime element_ jsonEditorRef trigger triggerEvent jsonable
                Just ref -> onNextTime  ref                    trigger jsonable

        onFirstTime :: (IsElement el)
                    => el
                    -- ^ Element
                    -> IORef (Maybe JsonEditorRef)
                    -- ^ Local state
                    -> (Maybe a -> IO ())
                    -- ^ Trigger for output event
                    -> (JsonEditorEvent -> IO ())
                    -- ^ Trigger for selection
                    -> a
                    -- ^ json data
                    -> IO ()
        onFirstTime element_ jsonEditorRef trigger triggerEvent jsonable_ = do
            ref <- newJsonEditor element_
                                 options
                                 (JsonEditor trigger
                                             triggerEvent
                                 )
            writeIORef jsonEditorRef (Just ref)
            set ref jsonable_
            expandAll ref
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
            expandAll jsonEditorRef
            trigger (Just jsonable_)
            return ()
