{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import                            Prelude hiding (head)
import           "lens"           Control.Lens
import           "data-default"   Data.Default (def)
import           "aeson"          Data.Aeson (toJSON)
import           "text"           Data.Text (Text)
import qualified "text"           Data.Text as T (lines, splitOn)
import           "reflex-dom"     Reflex.Dom
import           "reflex-utils"   Reflex.Utils
import           "reflex-jexcel"  Reflex.JExcel
import           "reflex-fileapi" Reflex.FileAPI.FileAPI

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
                     ]
    whenLoaded s1Ds blank $ do
        s2Ds <- sequence [ script "https://cdnjs.cloudflare.com/ajax/libs/jexcel/1.5.4/js/jquery.jexcel.js"
                         , css    "https://cdnjs.cloudflare.com/ajax/libs/jexcel/1.5.4/css/jquery.jexcel.min.css"
                         ]
        whenLoaded s2Ds blank blank
        return ()

--
body :: MonadWidget t m => m ()
body = do
    -- button triggers everything
    clickE  <- button "Load"

    -- filereader is triggered by clickE
    fileChunkTextE <- filereader stepSize clickE

    -- csvE :: Event t [[Text]]
    let csvE = (map (T.splitOn ",") . T.lines) <$> fileChunkTextE
    csvD <- holdDyn [] csvE

    -- spreadsheet
    let jexcelD = buildJExcel <$> csvD
    _ <- jexcel (JExcelInput "excel1" jexcelD)

    -- text display
    display csvD

    return ()
    where
        stepSize :: Int
        stepSize = 40000 -- bytes

        buildJExcel :: [[Text]] -> JExcel
        buildJExcel [] = def
        buildJExcel (headers:rows)
            = def
            & jExcel_colHeaders ?~ headers
            & jExcel_colWidths  ?~ (map (\_ -> 100) headers)
            & jExcel_data       ?~ rows
