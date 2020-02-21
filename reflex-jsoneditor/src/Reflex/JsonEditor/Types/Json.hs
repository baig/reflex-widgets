{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.JsonEditor.Types.Json where

import "aeson"        Data.Aeson.TH
import                Reflex.JsonEditor.Types.Types

$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_jsonEditorEvent_")} ''JsonEditorEvent)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_jsonEditorSelection_")} ''JsonEditorSelection)
$(deriveJSON defaultOptions{constructorTagModifier = fstToLower} ''JsonEditorMode)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_jsonEditorOptions_")} ''JsonEditorOptions)
