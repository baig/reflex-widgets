{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.JsonEditor.Types.Lens where

import Control.Lens
import Reflex.JsonEditor.Types.Types

makeClassyPrisms ''JsonEditorMode
makeClassy ''JsonEditorOptions
