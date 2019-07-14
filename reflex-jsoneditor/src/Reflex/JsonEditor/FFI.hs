{-# LANGUAGE PackageImports #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Reflex.JsonEditor.FFI where

import "lens"      Control.Lens hiding (element, (#))
import "aeson"     Data.Aeson (ToJSON, FromJSON, toJSON)
import "jsaddle"   Language.Javascript.JSaddle
import             GHCJS.DOM.Element (IsElement, toElement, unElement)
import             Reflex.JsonEditor.Types

newtype JsonEditorRef = JsonEditorRef
                      { unJsonEditorRef :: JSVal
                      }

--
newJsonEditor :: (IsElement element)
              => element
              -- ^ Element to use
              -> JsonEditorOptions
              -- ^ options
              -> IO JsonEditorRef
newJsonEditor element' options = do
    let js_element = unElement . toElement $ element'
    let js_options = toJSON $ options
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
