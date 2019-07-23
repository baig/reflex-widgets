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
import "reflex-jdenticon"  Reflex.Jdenticon

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
    s1Ds <- sequence [ script "https://cdn.jsdelivr.net/npm/jdenticon@2.2.0"
                     ]
    whenLoaded s1Ds blank $ return ()

--
body :: MonadWidget t m => m ()
body = do
    -- text input
    ti <- textInput def

    -- jdenticon
    _ <- jdenticon (constDyn (100, 100))
                   (_textInput_value ti)
    return ()
