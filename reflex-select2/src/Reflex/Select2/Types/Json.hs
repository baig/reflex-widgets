{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.Select2.Types.Json where

import "base"           Data.Char (toLower)
import "aeson"          Data.Aeson.TH
import                  Reflex.Select2.Types.Types

$(deriveJSON defaultOptions{constructorTagModifier = map toLower} ''Select2Width)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_select2Entry_")} ''Select2Entry)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_select2_")} ''Select2)
