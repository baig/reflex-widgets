{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.ChartJS.Types.Json where

import "aeson"        Data.Aeson.TH
import                Reflex.ChartJS.Types.Types

-- TODO: fix options


$(deriveJSON defaultOptions{constructorTagModifier = fstToLower} ''ChartJsType)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_chartJsDataset_")} ''ChartJsDataset)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_chartJsOptions_")} ''ChartJsOptions)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_chartJsLegend_")} ''ChartJsLegend)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_chartJsLabels_")} ''ChartJsLabels)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_chartJsScales_")} ''ChartJsScales)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_chartJsTooltips_")} ''ChartJsTooltips)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_chartJsAxis_")} ''ChartJsAxis)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_chartJsGridlines_")} ''ChartJsGridlines)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_chartJsTicks_")} ''ChartJsTicks)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_chartJsData_")} ''ChartJsData)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_chartJs_")} ''ChartJs)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_chartJsSelection_")} ''ChartJsSelection)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_scatterPoint_")} ''ScatterPoint)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_bubblePoint_")} ''BubblePoint)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_boxplotItem_")} ''BoxplotItem)
