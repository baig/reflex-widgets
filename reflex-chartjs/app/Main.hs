{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import                  Prelude hiding (head)
import "lens"           Control.Lens
import "data-default"   Data.Default (def)
import "aeson"          Data.Aeson (toJSON)
import "text"           Data.Text (Text)
import "reflex-dom"     Reflex.Dom
import "reflex-utils"   Reflex.Utils
import "reflex-chartjs" Reflex.ChartJS.ChartJS

--
main :: IO ()
main = mainWidget main_
    where
        main_ :: forall t m. MonadWidget t m => m ()
        main_ = do
            headD <- head
            whenLoaded [headD] blank body
            return ()

--
head :: forall t m. MonadWidget t m => m (Dynamic t Bool)
head = do
    s1Ds <- sequence [ script "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.3.1/jquery.min.js"
                     , script "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.5.0/Chart.bundle.min.js"
                     ]
    whenLoaded s1Ds blank $ return ()

--
body :: MonadWidget t m => m ()
body = do
    countE <- button "click"
    countD <- count countE
    display countD
    resultD <- holdDyn def =<< chartjs chartCallbacks
                                       (constDyn def)
                                       (chart <$> countD)
    display resultD
    resultD <- holdDyn def =<< chartjs chartCallbacks2
                                       (constDyn def)
                                       (chart <$> countD)
    display resultD
    return ()
    where
        chartCallbacks :: ChartJsCallbacks
        chartCallbacks
            = def
            {-& chartJsCallbacks_yAxisFormatter ?~ usdAxisFormatter-}

        chartCallbacks2 :: ChartJsCallbacks
        chartCallbacks2
            = def
            {-& chartJsCallbacks_yAxisFormatter ?~ percentAxisFormatter-}

        chart :: Int -> ChartJs
        chart n = def
                & chartJs_type .~ Bar
                & chartJs_data .~ chartData n
                & chartJs_options .~ chartOptions

        chartData :: Int -> ChartJsData
        chartData n = def
                    & chartJsData_labels   .~ (Just (toJSON ["a" :: Text, "b", "c", "d"]))
                    & chartJsData_xLabels  .~ (Just (toJSON ["a" :: Text, "b", "c", "d"]))
                    & chartJsData_datasets .~ [chartDataset n]

        chartOptions :: ChartJsOptions
        chartOptions
            = def
            & chartJsOptions_maintainAspectRatio ?~ False
            & chartJsOptions_legend . non def . chartJsLegend_display ?~ False
            & chartJsOptions_scales . non def . chartJsScales_xAxes   ?~ [chartJsAxisX]
            & chartJsOptions_scales . non def . chartJsScales_yAxes   ?~ [chartJsAxisY]
            where
                chartJsAxisX :: ChartJsAxis
                chartJsAxisX = def
                    & chartJsAxis_barPercentage ?~ 0.75

                chartJsAxisY :: ChartJsAxis
                chartJsAxisY = def
                    & chartJsAxis_stacked ?~ True

        chartDataset :: Int -> ChartJsDataset
        chartDataset n = def
                       & chartJsDataset_label ?~ "Example"
                       & chartJsDataset_data .~ (toJSON [1 :: Int, n, 3, 4])
                       & chartJsDataset_borderWidth ?~ (toJSON (1 :: Int))
                       & chartJsDataset_backgroundColor ?~ (toJSON [ "rgba(255, 99, 132, 0.2)" :: Text
                                                                   , "rgba(54, 162, 235, 0.2)"
                                                                   , "rgba(255, 206, 86, 0.2)"
                                                                   , "rgba(75, 192, 192, 0.2)"
                                                                   ]
                                                           )
                       & chartJsDataset_borderColor ?~ (toJSON [ "rgba(255,99,132,1)" :: Text
                                                               , "rgba(54, 162, 235, 1)"
                                                               , "rgba(255, 206, 86, 1)"
                                                               , "rgba(75, 192, 192, 1)"
                                                               ]
                                                       )

