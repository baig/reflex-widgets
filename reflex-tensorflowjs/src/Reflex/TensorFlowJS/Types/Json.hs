{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.TensorFlowJS.Types.Json where

import "aeson"        Data.Aeson.TH
import                Reflex.TensorFlowJS.Types.Types
