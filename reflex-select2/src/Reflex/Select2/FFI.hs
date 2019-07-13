{-# LANGUAGE PackageImports #-}
{-# LANGUAGE JavaScriptFFI #-}
module Reflex.Select2.FFI where

import "text"            Data.Text (Text)
import "ghcjs-base"      GHCJS.Types (JSVal)
import "ghcjs-base"      GHCJS.Marshal (toJSVal_aeson)
import "ghcjs-base"      GHCJS.Foreign.Callback (Callback, syncCallback1, OnBlocked(..))
import "ghcjs-dom-jsffi" GHCJS.DOM.Types (unElement)
import "ghcjs-dom-jsffi" GHCJS.DOM.Element (IsElement, toElement)
import                   Reflex.Select2.Types


newtype Select2Ref = Select2Ref
                   { unSelect2Ref :: JSVal
                   }

--
newSelect2 :: (IsElement element)
           => element
           -- ^ Element to use
           -> Select2
           -- ^ Select2 options
           -> IO Select2Ref
newSelect2 element_
           select2_ = do
    let js_element = unElement . toElement $ element_
    js_select2_ <- toJSVal_aeson select2_
    ref <- js_newSelect2 js_element
                         js_select2_
    return ref

foreign import javascript unsafe
    "(function() {                                                      \
    \   var ref = jQuery($1).empty().select2($2);                       \
    \   var performingReopen = false;                                   \
    \                                                                   \
    \   ref.on( 'select2:open', function() {                            \
    \                                                                   \
	\       if ( performingReopen ) {                                   \
	\           performingReopen = false;                               \
	\           return;                                                 \
	\       }                                                           \
    \                                                                   \
	\       performingReopen = true;                                    \
	\       ref.select2( 'close' );                                     \
	\       ref.select2( 'open' );                                      \
    \   });                                                             \
    \   return ref;                                                     \
    \})()"
    js_newSelect2 :: JSVal
                  -- ^ Element
                  -> JSVal
                  -- ^ Select2
                  -> IO Select2Ref
-- IMPORTANT Note: the code above is a workaround for safari taken from
-- https://github.com/select2/select2/issues/4678


-- -- TODO: did not work. maybe need to recreate the object
-- update :: Select2Ref
--             -- ^ Select2 ref
--             -> Select2
--             -- ^ Select2 data
--             -> IO ()
-- update ref select2_ = do
--     js_data <- toJSVal_aeson (select2_ ^. select2_data)
--     js_update ref js_data

-- foreign import javascript unsafe
--     "(function() {                      \
--     \    $1.updateOptions({range: $2}); \
--     \})()"
--     js_update :: NoUiSliderRef
--               -- ^ Select2 ref
--               -> JSVal
--               -- ^ Select2 range
--               -> IO ()


--
change :: Select2Ref
       -> Maybe [Text]
       -> IO ()
change select2Ref options = do
    js_options <- toJSVal_aeson options
    js_change select2Ref
              js_options

foreign import javascript unsafe
    "(function() {                       \
    \   if ($2) {                        \
    \      $1.val($2).trigger('change'); \
    \   }                                \
    \})()"
    js_change :: Select2Ref
              -> JSVal
              -> IO ()

--
onChange :: Select2Ref
         -> (JSVal -> IO ())
         -> IO ()
onChange select2Ref
         onChangeCallback = do
    js_onChangeCallback <- syncCallback1 ContinueAsync onChangeCallback
    js_onChange select2Ref
                js_onChangeCallback

foreign import javascript unsafe
    "(function() {                   \
    \   $1.on('change', function() { \
    \       $2($1.val());            \
    \   });                          \
    \})()"
    js_onChange :: Select2Ref
                -> Callback (JSVal -> IO ())
                -> IO ()
