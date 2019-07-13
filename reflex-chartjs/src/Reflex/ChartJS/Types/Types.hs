{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Reflex.ChartJS.Types.Types where

import "base"       GHC.Generics (Generic)
import "base"       Data.Data (Data)
import "base"       Data.Typeable (Typeable)
import "base"       Data.Char (toLower)
import "jsaddle"    Language.Javascript.JSaddle (JSVal)
import "text"       Data.Text (Text)
import "aeson"      Data.Aeson as Aeson (Value)

-- non-standard
type AxisFormatter    = (JSVal -> IO JSVal)
type TooltipFormatter = (JSVal -> IO JSVal)

data ChartJsCallbacks
    = ChartJsCallback
    { _chartJsCallbacks_xAxisFormatter   :: !(Maybe AxisFormatter)
    , _chartJsCallbacks_yAxisFormatter   :: !(Maybe AxisFormatter)
    , _chartJsCallbacks_tooltipFormatter :: !(Maybe TooltipFormatter)
    } deriving (Typeable, Generic)


--
data ChartJsType = Line
                 | Bar
                 | HorizontalBar
                 | Radar
                 | PolarArea
                 | Pie
                 | Doughnut
                 | Bubble
                 | Boxplot
                 | HorizontalBoxplot
                 | Violin
                 | HorizonalViolin
                 deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

--
data ChartJs = ChartJs
            { _chartJs_type    :: !ChartJsType
            , _chartJs_data    :: !ChartJsData
            , _chartJs_options :: !ChartJsOptions
            } deriving (Show, Read, Eq, Data, Typeable, Generic)

--
data ChartJsData = ChartJsData
                { _chartJsData_datasets :: !([ChartJsDataset])
                , _chartJsData_labels   :: !(Maybe Aeson.Value)
                , _chartJsData_xLabels  :: !(Maybe Aeson.Value)
                , _chartJsData_yLabels  :: !(Maybe Aeson.Value)
                } deriving (Show, Read, Eq, Data, Typeable, Generic)

--
type ColorOrColors = Aeson.Value
{-
data ColorOrColors = Color Text
                   | Colors [Text]
                   deriving (Show, Read, Eq, Data, Typeable, Generic)
-}
--
type NumberOrNumbers = Aeson.Value
{-
data NumberOrNumbers = Number Int
                     | Numbers [Int]
                     deriving (Show, Read, Eq, Data, Typeable, Generic)
-}


--
data ChartJsOptions = ChartJsOptions
                    { _chartJsOptions_legend              :: !(Maybe ChartJsLegend)
                    , _chartJsOptions_scales              :: !(Maybe ChartJsScales)
                    , _chartJsOptions_maintainAspectRatio :: !(Maybe Bool)
                    , _chartJsOptions_tooltips            :: !(Maybe ChartJsTooltips)
                    } deriving (Show, Read, Eq, Data, Typeable, Generic)


data ChartJsLegend = ChartJsLegend
                   { _chartJsLegend_display :: !(Maybe Bool)
                   , _chartJsLegend_labels  :: !(Maybe ChartJsLabels)
                   } deriving (Show, Read, Eq, Data, Typeable, Generic)

data ChartJsLabels = ChartJsLabels
                   { _chartJsLabels_boxWidth   :: !(Maybe Int)
                   , _chartJsLabels_fontSize   :: !(Maybe Int)
                   , _chartJsLabels_fontStyle  :: !(Maybe String)
                   , _chartJsLabels_fontColor  :: !(Maybe String)
                   , _chartJsLabels_fontFamily :: !(Maybe String)
                   , _chartJsLabels_padding    :: !(Maybe Int)
                   } deriving (Show, Read, Eq, Data, Typeable, Generic)

data ChartJsScales = ChartJsScales
                   { _chartJsScales_xAxes :: !(Maybe [ChartJsAxis])
                   , _chartJsScales_yAxes :: !(Maybe [ChartJsAxis])
                   } deriving (Show, Read, Eq, Data, Typeable, Generic)

data ChartJsTooltips = ChartJsTooltips
                     { _chartJsTooltips_enabled   :: !(Maybe Bool)
                     , _chartJsTooltips_callbacks :: !(Maybe Value)
                     } deriving (Show, Read, Eq, Data, Typeable, Generic)

data ChartJsAxis = ChartJsAxis
                 { _chartJsAxis_stacked            :: !(Maybe Bool)
                 , _chartJsAxis_categoryPercentage :: !(Maybe Double)
                 , _chartJsAxis_barPercentage      :: !(Maybe Double)
                 , _chartJsAxis_barThickness       :: !(Maybe Int)
                 , _chartJsAxis_gridLines          :: !(Maybe ChartJsGridlines)
                 , _chartJsAxis_ticks              :: !(Maybe ChartJsTicks)
                 , _chartJsAxis_id                 :: !(Maybe Text)
                 , _chartJsAxis_display            :: !(Maybe Bool)
                 } deriving (Show, Read, Eq, Data, Typeable, Generic)


data ChartJsGridlines = ChartJsGridlines
                      { _chartJsGridlines_display    :: !(Maybe Bool)
                      , _chartJsGridlines_drawBorder :: !(Maybe Bool)
                      } deriving (Show, Read, Eq, Data, Typeable, Generic)

data ChartJsTicks = ChartJsTicks
                  { _chartJsTicks_width :: !(Maybe Int)
                  } deriving (Show, Read, Eq, Data, Typeable, Generic)

--
data ChartJsDataset = ChartJsDataset
                     { _chartJsDataset_data            :: !Aeson.Value
                     , _chartJsDataset_label           :: !(Maybe Text)
                     , _chartJsDataset_xAxisID         :: !(Maybe Text)
                     , _chartJsDataset_yAxisID         :: !(Maybe Text)
                     , _chartJsDataset_backgroundColor :: !(Maybe ColorOrColors)
                     , _chartJsDataset_borderColor     :: !(Maybe ColorOrColors)
                     , _chartJsDataset_borderWidth     :: !(Maybe NumberOrNumbers)
                     , _chartJsDataset_fill            :: !(Maybe Bool)
                     , _chartJsDataset_lineTension     :: !(Maybe Int)
                     , _chartJsDataset_borderDash      :: !(Maybe [Int])


                     -- Line
                     , _chartJsDataset_cubicInterpolationMode :: !(Maybe String)
                     , _chartJsDataset_borderCapStyle   :: !(Maybe String)
                     , _chartJsDataset_borderDashOffset :: !(Maybe Int)
                     , _chartJsDataset_borderJoinStyle  :: !(Maybe String)
                     --    some more
                     , _chartJsDataset_showLine         :: !(Maybe Bool)
                     , _chartJsDataset_spanGaps         :: !(Maybe Bool)
                     , _chartJsDataset_steppedLine      :: !(Maybe Bool)

                     -- boxplot
                     , _chartJsDataset_outlierColor     :: !(Maybe Text)
                     } deriving (Show, Read, Eq, Data, Typeable, Generic)

--
data ChartJsSelection = ChartJsSelection
                  { _chartJsSelection_datasetLabel :: !(Maybe Text)
                  , _chartJsSelection_datasetIndex :: !(Maybe Int)
                  , _chartJsSelection_label        :: !(Maybe Text)
                  , _chartJsSelection_index        :: !(Maybe Int)
                  , _chartJsSelection_value        :: !(Maybe Double)
                  } deriving (Show, Read, Eq, Data, Typeable, Generic)

data ScatterPoint = ScatterPoint
                  { _scatterPoint_x :: Int
                  , _scatterPoint_y :: Double
                  } deriving (Show, Read, Eq, Data, Typeable, Generic)

data BubblePoint = BubblePoint
                 { _bubblePoint_x :: !Double
                 , _bubblePoint_y :: !Double
                 , _bubblePoint_r :: !Double
                 } deriving (Show, Read, Eq, Data, Typeable, Generic)

data BoxplotItem = BoxplotItem
                 { _boxplotItem_min      :: !Double
                 , _boxplotItem_q1       :: !Double
                 , _boxplotItem_median   :: !Double
                 , _boxplotItem_q3       :: !Double
                 , _boxplotItem_max      :: !Double
                 , _boxplotItem_outliers :: ![Double]
                 } deriving (Show, Read, Eq, Data, Typeable, Generic)

fstToLower :: String -> String
fstToLower (x:xs) = (toLower x) : xs
fstToLower _      = ""
