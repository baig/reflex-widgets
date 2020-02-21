{-# LANGUAGE PackageImports #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Reflex.JsonEditor.FFI where

import "lens"        Control.Lens hiding (element, (#))
import "aeson"       Data.Aeson (ToJSON, FromJSON, toJSON)
import "jsaddle"     Language.Javascript.JSaddle
import               GHCJS.DOM.Element (IsElement, toElement, unElement)
import               Reflex.JsonEditor.Types



newtype JsonEditorRef = JsonEditorRef
                      { unJsonEditorRef :: JSVal
                      }

class JsonEditorHandlers a where
    onChangeJSON :: a -> JSVal -> JSM ()
    onChangeJSON  _ _ = return ()

    onEvent :: a -> JSVal -> JSVal -> JSM ()
    onEvent _ _ _ = return ()

    onSelectionChange :: a -> [JSVal] -> JSM ()
    onSelectionChange _ _ = return ()

--
newJsonEditor :: ( IsElement element
                 , JsonEditorHandlers handlers
                 )
              => element
              -- ^ Element to use
              -> JsonEditorOptions
              -- ^ options
              -> handlers
              -- ^ handlers
              -> IO JsonEditorRef
newJsonEditor element'
              options
              handlers
    = do
    let js_element = unElement . toElement $ element'
    js_options <- toJSVal . toJSON $ options
    js_options ^. jss "onChangeJSON" (fun $ \_ _ [jsval'] -> onChangeJSON handlers $ jsval')
    js_options ^. jss "onEvent" (fun $ \_ _ [node, event] -> onEvent handlers node event)
    js_options ^. jss "onSelectionChange" (fun $ \_ _ xs -> onSelectionChange handlers xs)
    jsoneditor <- nextAnimationFrame $ \_ -> new (jsg "JSONEditor") (js_element, js_options)
    return $ JsonEditorRef jsoneditor

--
set :: (ToJSON a, FromJSON a)
    => JsonEditorRef
    -- ^ Ref
    -> a
    -- ^ value
    -> JSM ()
set ref value = do
    jsoneditor <- valToObject . unJsonEditorRef $ ref
    _ <- jsoneditor ^. js1 "set" (toJSON value)
    return ()

--
expandAll :: JsonEditorRef
          -- ^ Ref
          -> JSM ()
expandAll ref = do
    jsoneditor <- valToObject . unJsonEditorRef $ ref
    _ <- jsoneditor ^. js0 "expandAll"
    return ()

--
getSelection :: JsonEditorRef
             -- ^ Ref
             -> JSM JSVal
getSelection ref = do
    jsoneditor <- valToObject . unJsonEditorRef $ ref
    selection  <- jsoneditor ^. js0 "getSelection"
    return selection
