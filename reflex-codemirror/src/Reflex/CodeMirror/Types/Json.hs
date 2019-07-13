{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.CodeMirror.Types.Json where

import "aeson" Data.Aeson.TH
import         Reflex.CodeMirror.Types.Types

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_configuration_")} ''Configuration)
