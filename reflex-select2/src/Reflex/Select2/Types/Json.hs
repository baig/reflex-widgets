{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.Select2.Types.Json where

import "base"           Data.Char (toLower)
import "aeson"          Data.Aeson.TH
import "common-types"   Atidot.Common.Types
import                  Reflex.Select2.Types.Types

$(deriveJSON defaultOptions{constructorTagModifier = map toLower} ''Select2Width)
$(deriveJSON (atidotDefaultJsonOptions (undefined :: Select2Entry)) ''Select2Entry)
$(deriveJSON (atidotDefaultJsonOptions (undefined :: Select2)) ''Select2)
