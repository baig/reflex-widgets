{-# LANGUAGE PackageImports #-}
{-# LANGUAGE JavaScriptFFI #-}
module Reflex.Select2.FFI where

import "lens"      Control.Lens hiding (element, (#))
import "aeson"     Data.Aeson (toJSON)
import "text"      Data.Text (Text)
import "jsaddle"   Language.Javascript.JSaddle
import             GHCJS.DOM.Element (IsElement, toElement, unElement)
import             Reflex.Select2.Types


newtype Select2Ref = Select2Ref
                   { unSelect2Ref :: JSVal
                   }

--
newSelect2 :: (IsElement element)
           => element
           -- ^ Element to use
           -> Select2
           -- ^ Select2 options
           -> JSM Select2Ref
newSelect2 element_
           select2_ = do
    let js_element  = unElement . toElement $ element_
    let js_select2_ = toJSON $ select2_
    ref  <- nextAnimationFrame $ \_ -> do
        tmp1 <- jsgf "jQuery" js_element
        tmp2 <- tmp1 ^. js0 "empty"
        tmp2 ^. js1 "select2" js_select2_
    return $ Select2Ref ref


--
update :: Select2Ref
       -- ^ ref
       -> [Text]
       -- ^ texts
       -> JSM ()
update ref texts = do
    s2 <- valToObject . unSelect2Ref $ ref
    _ <- s2 ^. js1 "val" texts
    _ <- s2 ^. js1 "trigger" "change"
    return ()
