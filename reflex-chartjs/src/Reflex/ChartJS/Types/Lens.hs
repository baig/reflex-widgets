{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.ChartJS.Types.Lens where

import Control.Lens
import Reflex.ChartJS.Types.Types

makeClassy ''ChartJsCallbacks
makeClassyPrisms ''ChartJsType
makeClassy ''ChartJsDataset
makeClassy ''ChartJsData
makeClassy ''ChartJsOptions
makeClassy ''ChartJsLegend
makeClassy ''ChartJsLabels
makeClassy ''ChartJsScales
makeClassy ''ChartJsTooltips
makeClassy ''ChartJsAxis
makeClassy ''ChartJsGridlines
makeClassy ''ChartJsTicks
makeClassy ''ChartJs
makeClassy ''ChartJsSelection
makeClassy ''ScatterPoint
makeClassy ''BubblePoint
