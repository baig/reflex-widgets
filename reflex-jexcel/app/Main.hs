{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import                                  Prelude hiding (head)
import           "lens"                 Control.Lens
import           "text"                 Data.Text (pack)
import           "reflex-dom"           Reflex.Dom
import           "reflex-utils"         Reflex.Utils
import           "reflex-jexcel"        Reflex.JExcel

--
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
body :: forall t m. MonadWidget t m => m ()
body = do
    -- counter
    bE <- button "next"
    counterD <- count bE
    display counterD

    -- JExcel configuration (Dynamic)
    let jexcelD = buildJExcel <$> counterD

    -- jExcel
    jexcelOutput <- jexcel (JExcelInput htmlId jexcelD)

    -- transform input to output
    let xE' = _jexcelOutput_event jexcelOutput
    let xE = ffilter isSelection xE'
    xD <- holdDyn (OnLoad) xE
    display xD

    return ()

    where
        isSelection :: JExcelEvent -> Bool
        isSelection (OnSelection _ _ ) = False
        isSelection _                  = True

        htmlId  = "excel1"

        defaultJExcel :: JExcel
        defaultJExcel = def
                      & jExcel_colHeaders ?~ ["First Name", "Last Name", "Premium", "Zipcode"]
                      & jExcel_colWidths  ?~ [300, 80, 100, 100]

        buildJExcel :: Int -> JExcel
        buildJExcel n = defaultJExcel
                      & jExcel_data ?~ [ ["John" , "Doe"    , pack . show $ n  , "90210"]
                                       , ["Jane" , "Doe"    , "$2000"          , "10010"]
                                       , ["Johan", "Though" , "$3000"          , "20020"]
                                       ]
