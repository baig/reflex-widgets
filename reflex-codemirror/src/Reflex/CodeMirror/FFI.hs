{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.CodeMirror.FFI where

import "ghcjs-base"      GHCJS.Types (JSVal)
import "ghcjs-base"      GHCJS.Marshal (toJSVal_aeson)
import "ghcjs-base"      GHCJS.Foreign.Callback (Callback, syncCallback1, OnBlocked(..))
{-import "ghcjs-base"      GHCJS.Marshal (fromJSVal)-}
import "ghcjs-dom-jsffi" GHCJS.DOM.Types (unElement)
import "ghcjs-dom-jsffi" GHCJS.DOM.Element (IsElement, toElement)
import                   Reflex.CodeMirror.Types

newtype CodeMirrorRef = CodeMirrorRef
                      { unCodeMirrorRef :: JSVal
                      }

--
fromTextArea :: (IsElement element)
             => element
             -- ^ Element to use
             -> Configuration
             -- ^ CodeMirror configuration
             -> IO CodeMirrorRef
fromTextArea element_
             config = do
    let js_element = unElement . toElement $ element_
    js_config <- toJSVal config
    ref <- js_fromTextArea js_element
                           js_config
    return ref


foreign import javascript unsafe
    "(function() {                                 \
    \    var cm = CodeMirror.fromTextArea($1, $2); \
    \    if ($2.value) {                           \
    \       cm.setValue($2.value);                 \
    \    }                                         \
    \    return cm;                                \
    \})()"
    js_fromTextArea :: JSVal
                    -- ^ Element
                    -> JSVal
                    -- ^ CodeMirror configuratino
                    -> IO CodeMirrorRef

--
onChange :: CodeMirrorRef
         -> (JSVal -> IO ())
         -> IO ()
onChange ref
         callback = do
    js_callback <- syncCallback1 ContinueAsync callback
    js_onChange ref
                js_callback

foreign import javascript unsafe
    "(function() {                        \
    \    $1.on('change', function(obj) {  \
    \        $2($1.getDoc().getValue());  \
    \    });                              \
    \})()"
    js_onChange :: CodeMirrorRef
                -> Callback (JSVal -> IO ())
                -> IO ()
