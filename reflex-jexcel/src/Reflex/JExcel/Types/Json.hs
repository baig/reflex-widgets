{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.JExcel.Types.Json where

import "base"         Data.Char (toLower)
import "lens"         Control.Lens (over, _head)
import "aeson"        Data.Aeson.TH
import                Reflex.JExcel.Types.Types

$(deriveJSON defaultOptions{constructorTagModifier = over _head toLower . drop 6} ''JExcelAlignment)
$(deriveJSON defaultOptions{constructorTagModifier = map toLower} ''JExcelEvent)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_jexcel_")} ''JExcel)
