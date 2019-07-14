{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}

module Reflex.Utils.Image ( image
                          , imageDyn
                          ) where

import "base"                   Data.Monoid ((<>))
import "reflex-dom"             Reflex.Dom
import "filepath"               System.FilePath
import qualified "text"         Data.Text as T
import qualified "containers"   Data.Map as M


imageDyn :: forall t m. (MonadWidget t m)
       => Dynamic t String
       -> Maybe String
       -> m ()
imageDyn nameD mClass = elDynAttr "img" (attr <$> nameD) blank
  where
    classAttr = maybe M.empty (\class' -> "class" =: T.pack class') mClass
    attr name =
            ( classAttr
           <> "src" =: T.pack (imagePath name)
            )
    imagePath name = staticImgPath </> name


image :: forall t m. (MonadWidget t m)
      => String
      -> Maybe String
      -> m ()
image imgName mClass = imageDyn (constDyn imgName) mClass

staticImgPath :: FilePath
staticImgPath = "static/img"
