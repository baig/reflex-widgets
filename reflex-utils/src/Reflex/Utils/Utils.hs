{-# LANGUAGE PackageImports #-}
module Reflex.Utils.Utils where

import "reflex" Reflex

-- we filter out events that we created automatically whe setting the original value
updateOnChange :: (Reflex t, Eq a)
               => Dynamic t a
               -> Dynamic t a
               -> Event t a
updateOnChange originalD newD = tagPromptlyDyn newD updatedE
    where
        updatedD = (/=) <$> originalD <*> newD
        updatedE = ffilter id (updated updatedD)
