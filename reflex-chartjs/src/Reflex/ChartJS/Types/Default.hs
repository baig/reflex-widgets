{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.ChartJS.Types.Default where


import "data-default" Data.Default
import "aeson"        Data.Aeson as Aeson (Value(..))
import "derive"       Data.DeriveTH (derive, makeDefault)
import                Reflex.ChartJS.Types.Types

instance Default Value where
    def = Null

$(derive makeDefault ''ChartJsCallbacks)
$(derive makeDefault ''ChartJsType)
$(derive makeDefault ''ChartJs)
$(derive makeDefault ''ChartJsData)
$(derive makeDefault ''ChartJsDataset)
$(derive makeDefault ''ChartJsSelection)
$(derive makeDefault ''ChartJsOptions)
$(derive makeDefault ''ChartJsLegend)
$(derive makeDefault ''ChartJsLabels)
$(derive makeDefault ''ChartJsTooltips)
$(derive makeDefault ''ChartJsScales)
$(derive makeDefault ''ChartJsAxis)
$(derive makeDefault ''ChartJsGridlines)
$(derive makeDefault ''ChartJsTicks)
