{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE MonoLocalBinds #-}

module Reflex.Utils.Dom where

import "reflex-dom"   Reflex.Dom
-- input event -- this code block is intended for modifying the object for the first time after compilation. when no other event has yet to occur.
-- it can be looked upon as:
-- if first_time then do `tagPromptlyDyn aD postBuildE` else updated aD
createdOrUpdated :: MonadWidget t m
                 => Dynamic t a
                 -> m (Event t a)
createdOrUpdated aD = do
    postBuildE <- getPostBuild
    return $ leftmost
        [ updated aD
        , tagPromptlyDyn aD postBuildE
        ]
