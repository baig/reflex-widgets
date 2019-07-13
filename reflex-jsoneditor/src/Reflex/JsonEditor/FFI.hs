{-# LANGUAGE PackageImports #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Reflex.JsonEditor.FFI where



import             Prelude hiding ((!!))
import "lens"      Control.Lens hiding (element, (#))
import "aeson"     Data.Aeson (toJSON, ToJSON, FromJSON)
import "jsaddle"   Language.Javascript.JSaddle
import             GHCJS.DOM.Element (IsElement, toElement, unElement)
import             Reflex.ChartJS.Types


newtype JsonEditorRef = JsonEditorRef
                      { unJsonEditorRef :: JSVal
                      }

--
newJsonEditor :: (ToJSON a, FromJSON a, IsElement element)
              => element
              -- ^ Element to use
              -> (Maybe a -> IO ())
              -- ^ trigger
              -> IO JsonEditorRef
newJsonEditor element_
              trigger = do
    let js_element = unElement . toElement $ element_
    jsoneditor <- nextAnimationFrame $ \_ -> new (jsg  "JSONEditor") (js_element, 

    js_onchange <- syncCallback1 ContinueAsync (onChange trigger)
    ref <- js_newJsonEditor js_element
                            js_onchange
    return ref


onChange :: (ToJSON a, FromJSON a)
         => (Maybe a -> IO ())
         -- ^ trigger
         -> JSVal
         -- ^ value
         -> IO ()
onChange trigger value = do
    Just (json_ :: Text) <- fromJSVal value
    let mEvent = decodeStrict . encodeUtf8 $ json_
    trigger mEvent


foreign import javascript unsafe
    "(function() {                                         \
    \    var container = $1;                               \
    \    var options = {                                   \
    \        onChange: function () {                       \
    \            $2(JSON.stringify(editor.get()));         \
    \        },                                            \
    \        mode: 'code'                                  \
    \    };                                                \
    \    var editor = new JSONEditor(container, options);  \
    \    return editor;                                    \ 
    \})()"
    js_newJsonEditor :: JSVal
                     -- ^ Element
                     -> Callback (JSVal -> IO ())
                     -- ^ on change
                     -> IO JsonEditorRef

--
set :: (ToJSON a, FromJSON a)
    => JsonEditorRef
    -- ^ ref
    -> a
    -- ^ value
    -> IO ()
set ref json = do
    js_json <- toJSVal_aeson json
    js_set ref js_json

foreign import javascript unsafe
    "(function() {    \
    \    $1.set($2);  \
    \})()"
    js_set :: JsonEditorRef
           -> JSVal
           -> IO ()
