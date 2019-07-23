{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}
module Reflex.Jdenticon ( jdenticon
                        ) where

import "base"            Data.Monoid ((<>))
import "lens"            Control.Lens
import "text"            Data.Text (Text, pack)
import "containers"      Data.Map (Map)
import "reflex-dom"      Reflex.Dom
import "jsaddle"         Language.Javascript.JSaddle
import                   GHCJS.DOM.Element (IsElement, toElement, unElement)


jdenticon :: MonadWidget t m
          => Dynamic t (Int, Int)
          -- ^ (Width, Height)
          -> Dynamic t Text
          -- ^ Hash
          -> m ()
jdenticon shapeD textD = do
    -- jdenticon element
    let attrsD = toAttrs <$> textD
                         <*> shapeD
    (element_, _) <- elDynAttr' "canvas" attrsD blank

    postBuildTaggedE <- tagPromptlyDyn textD <$> getPostBuild
    let inputE = leftmost
               [ postBuildTaggedE
               , updated textD
               ]

    -- handle input event
    ctxRef <- askJSM
    performEvent_ $ ffor inputE $ \text_ -> flip runJSM ctxRef $ update (_element_raw element_)
                                                                        text_

    return ()
    where
        toAttrs :: Text -> (Int, Int) -> Map Text Text
        toAttrs text_ (width, height) = ( "width"  =: (pack . show $ width)
                                       <> "height" =: (pack . show $ height)
                                       <> "data-jdenticon-value" =: text_
                                       <> "class" =: "atidot-jdenticon"
                                        )
update :: (IsElement element)
       => element
       -- ^ Element to use
       -> Text
       -- ^ value
       -> JSM ()
update element' value' = do
    let js_element = unElement . toElement $ element'
    js_jdenticon <- jsg ("jdenticon" :: Text)
    _ <- js_jdenticon ^. js2 ("update" :: Text) js_element value'
    return ()
