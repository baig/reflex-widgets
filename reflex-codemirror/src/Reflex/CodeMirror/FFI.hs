{-# LANGUAGE PackageImports #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.CodeMirror.FFI where

import "lens"      Control.Lens hiding (element, (#))
import "text"      Data.Text (Text)
import "aeson"     Data.Aeson (toJSON)
import "jsaddle"   Language.Javascript.JSaddle
import             GHCJS.DOM.Element (IsElement, toElement, unElement)
import             Reflex.CodeMirror.Types

newtype CodeMirrorRef = CodeMirrorRef
                      { unCodeMirrorRef :: JSVal
                      }

--
fromTextArea :: (IsElement element)
             => element
             -- ^ Element to use
             -> Configuration
             -- ^ CodeMirror configuration
             -> JSM CodeMirrorRef
fromTextArea element_
             config = do
    let js_element = unElement . toElement $ element_
    let js_config  = toJSON $ config
    ref <- nextAnimationFrame $ \_ -> (jsg "CodeMirror") # "fromTextArea" $ (js_element, js_config)
    return $ CodeMirrorRef ref


registerOnChange :: CodeMirrorRef
                 -- ^ ref
                 -> (Text-> JSM ())
                 -- ^ Callback with Text
                 -> JSM ()
registerOnChange codeMirrorRef callback = do
    codemirror <- valToObject . unCodeMirrorRef $ codeMirrorRef
    _ <- codemirror ^. js2 "on" "change" (fun $ \_ _ _ -> do
        x <- codemirror ^. js0 "getValue"
        w <- jsg "console"
        _ <- w ^. js1 "log" x
        t <- valToText x
        -- _ <- valToText =<< codemirror ^. js0 "getValue"
        callback t
        return ()
        )
    return ()
