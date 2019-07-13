{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}
module Reflex.MDL.Dialog where

import "base"            Data.Monoid ((<>))
import "base"            Control.Monad.IO.Class (liftIO)
import "base"            Control.Monad (void)
import "reflex-dom"      Reflex.Dom
import "ghcjs-base"      GHCJS.Types
import "ghcjs-dom-jsffi" GHCJS.DOM.Types (unElement)
import "ghcjs-dom-jsffi" GHCJS.DOM.Element (IsElement, toElement)
import                   Reflex.MDL.Common (materialize')
import                   Reflex.MDL.Button ( mdlButton
                                           , mdlButtonWithWidget
                                           )
import                   Reflex.MDL.Icon (mdlIcon, Icon(..))
import                   Reflex.MDL.Grid

mdlYesNo :: MonadWidget t m
          => Event t ()
          -> m a
          -- ^ Show trigger
          -> m (Event t Bool)
mdlYesNo showTriggerE widget = do -- widget
    -- dialog element
    (element_, (negativeE, positiveE)) <- do
        elAttr' "dialog" ( "class" =: "mdl-dialog"
                        <> "width" =: "fit-content"
                         ) $ do
            negativeE1 <- divClass "mdl-dialog__actions" $ do
                mdlButtonWithWidget (snd <$> mdlIcon False (constDyn IconClear))
                                    ["pull-right", "atidot-small-button"]

            mdlGrid $
                mdlCell 12 $
                     void widget

            mdlGrid $ do
                positiveE' <- mdlCell 6
                    $ mdlButton $ constDyn "Yes"
                negativeE2 <- mdlCell 6
                    $ mdlButton $ constDyn "No"
                return (leftmost [negativeE1, negativeE2], positiveE')

    --
    let controlE = leftmost
                 [ True  <$ showTriggerE
                 , False <$ leftmost [negativeE, positiveE]
                 ]

    -- handle events
    performEvent_ $ ffor controlE
                  $ \isOpen -> liftIO
                  $ onChange (_element_raw element_)
                             isOpen

    return $ leftmost [ True  <$ positiveE
                      , False <$ negativeE
                      ]

    where
        onChange :: (IsElement el)
                 => el
                 -- ^ Element
                 -> Bool
                 -- ^ Is open
                 -> IO ()
        onChange element_ False = close element_
        onChange element_ True  = showModal element_


mdlDialog :: MonadWidget t m
          => Event t ()
          -- ^ Show trigger
          -> m a
          -- ^ Widget
          -> m ()
mdlDialog showTriggerE widget = do
    -- dialog element
    (element_, closeE) <- materialize' $ do
        elAttr' "dialog" ( "class" =: "mdl-dialog"
                        <> "width" =: "fit-content"
                         ) $ do
            closeE1 <- divClass "mdl-dialog__actions" $ do
                mdlButtonWithWidget (snd <$> mdlIcon False (constDyn IconClear)) ["pull-right", "atidot-small-button"]

            _ <- divClass "mdl-dialog__actions" $ do
                widget

            closeE2 <- divClass "mdl-dialog__actions" $ do
                mdlButton $ constDyn "Close"

            return $ leftmost [closeE1, closeE2]

    --
    let controlE = leftmost
                 [ True  <$ showTriggerE
                 , False <$ closeE
                 ]

    -- handle events
    performEvent_ $ ffor controlE
                  $ \isOpen -> liftIO
                  $ onChange (_element_raw element_)
                             isOpen

    return ()

    where
        onChange :: (IsElement el)
                 => el
                 -- ^ Element
                 -> Bool
                 -- ^ Is open
                 -> IO ()
        onChange element_ False = close element_
        onChange element_ True  = showModal element_


--
-- FFI

showModal :: (IsElement element)
          => element
          -- ^ Element to bind to
          -> IO ()
showModal element_ = do
    let js_element = unElement . toElement $ element_
    js_showModal js_element

foreign import javascript unsafe
    "(function() {       \
    \    dialogPolyfill.registerDialog($1); \
    \    $1.showModal();                    \
    \})()"
    js_showModal :: JSVal
                 -- ^ Element to use
                 -> IO ()

close :: (IsElement element)
      => element
      -- ^ Element to bind to
      -> IO ()
close element_ = do
    let js_element = unElement . toElement $ element_
    js_close js_element

foreign import javascript unsafe
    "(function() {   \
    \    $1.close(); \
    \})()"
    js_close :: JSVal
             -- ^ Element to use
             -> IO ()
