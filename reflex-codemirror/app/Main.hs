{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import                     Prelude hiding (head)
import "lens"              Control.Lens
import "data-default"      Data.Default (def)
import "aeson"             Data.Aeson (toJSON)
import "text"              Data.Text (Text, pack)
import "reflex-dom"        Reflex.Dom
import "reflex-utils"      Reflex.Utils
import "reflex-codemirror" Reflex.CodeMirror

--
main :: IO ()
main = mainWidget main_
    where
        main_ :: forall t m. MonadWidget t m => m ()
        main_ = do
            headD <- head
            whenLoaded [headD] blank body
            return ()

--
head :: forall t m. MonadWidget t m => m (Dynamic t Bool)
head = do
    s1Ds <- sequence [ script "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.48.0/codemirror.min.js"
                     , css    "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.48.0/codemirror.min.css"
                     , css    "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.48.0/theme/zenburn.css"
                     ]
    whenLoaded s1Ds blank $ do
        sequence [ script "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.48.0/mode/haskell/haskell.min.js"
                 ]
        return ()

--
body :: MonadWidget t m => m ()
body = do
    textE <- codemirror config
    textD <- holdDyn "" textE
    display textD
    where
        config :: Configuration
        config
            = def
            & configuration_theme ?~ pack "zenburn"
            & configuration_mode  ?~ pack "haskell"

