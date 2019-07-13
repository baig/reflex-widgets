{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import                                  Prelude hiding (head)
import           "base"                 Control.Monad (void)
import           "base"                 Control.Monad.IO.Class (liftIO)
import           "base"                 Data.Char (toUpper)
import           "base"                 Data.Monoid ((<>))
import qualified "bytestring"           Data.ByteString.Char8 as B8
import           "containers"           Data.Map.Strict (fromList)
import           "lens"                 Control.Lens
import           "time"                 Data.Time.Clock (getCurrentTime)
import qualified "text"                 Data.Text as T
import           "text"                 Data.Text.Encoding (encodeUtf8)
import           "reflex-dom"           Reflex.Dom
import           "reflex-jexcel"        Reflex.JExcel

instance Semigroup Bool where
    b1 <> b2 = b1 && b2

instance Monoid Bool where
    b1 `mappend` b2 = b1 && b2
    mempty = True

whenLoaded :: forall t m. MonadWidget t m
           => [Dynamic t Bool]
           -- ^ Wait for these to load
           -> m ()
           -- ^ Loading widget
           -> m ()
           -- ^ Loaded widget
           -> m (Dynamic t Bool)
whenLoaded loadedDs
           loadingWidget
           loadedWidget = do
    allLoadedD <- return . mconcat $ loadedDs
    let allLoadedE = ffilter id .  updated $ allLoadedD
    delayedLoadedE <- delay 0.5 allLoadedE
    changeD <- widgetHold loadingWidget $ ffor delayedLoadedE $ \_ -> do
        loadedWidget
    changeE_ <- headE $ updated changeD
    let changeE = True <$ changeE_
    holdDyn False changeE


script :: forall t m. MonadWidget t m
       => T.Text
       -- ^ URI
       -> m (Dynamic t Bool)
script uri = do
    (element_ , _) <- elAttr' "script"
                              ("src" =: uri)
                              blank
    let loadedE = (True <$ domEvent Load element_)
    holdDyn False loadedE


css :: forall t m. MonadWidget t m
    => T.Text
    -- ^ URI
    -> m (Dynamic t Bool)
css uri = do
    (element_, _) <- elAttr' "link"
                             ( "rel"  =: "stylesheet"
                            <> "type" =: "text/css"
                            <> "href" =: uri
                             )
                             blank
    let loadedE = (True <$ domEvent Load element_)
    holdDyn False loadedE


main = mainWidget main_
    where
        main_ :: forall t m. MonadWidget t m => m ()
        main_ = do
            headD <- head
            whenLoaded [headD] blank body
            return ()


head :: forall t m. MonadWidget t m => m (Dynamic t Bool)
head = do
    s1Ds <- sequence [ script "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.3.1/jquery.min.js"
                     , css    "https://cdnjs.cloudflare.com/ajax/libs/material-design-lite/1.3.0/material.min.css"
                     , script "https://cdnjs.cloudflare.com/ajax/libs/material-design-lite/1.3.0/material.min.js"
                     , script "https://cdnjs.cloudflare.com/ajax/libs/numeral.js/2.0.6/numeral.min.js"
                     ]
    whenLoaded s1Ds blank $ do
        s2Ds <- sequence [ script "https://cdnjs.cloudflare.com/ajax/libs/jexcel/1.5.4/js/jquery.jexcel.js"
                         , css    "https://cdnjs.cloudflare.com/ajax/libs/jexcel/1.5.4/css/jquery.jexcel.min.css"
                         ]
        whenLoaded s2Ds blank blank
        return ()

bla :: JExcelEvent -> Bool
bla (OnSelection _ _ ) = False
bla _                   = True

body :: forall t m. MonadWidget t m => m ()
body = do
    bE <- button "next"
    counterD <- count bE
    let jexcelD = buildJExcel <$> counterD

    -- input jExcel
    jexcelOutput <- jexcel (JExcelInput htmlId jexcelD)

    -- transform input to output
    let xE' = _jexcelOutput_event jexcelOutput
    let xE = ffilter bla xE'
    xD <- holdDyn (OnLoad) xE
    display xD


    return ()

    where
        htmlId  = "excel1"
        htmlId2 = "excel2"

        defaultJExcel :: JExcel
        defaultJExcel = def
                      & jExcel_colHeaders ?~ ["First Name", "Last Name", "Premium", "Zipcode"]
                      & jExcel_colWidths  ?~ [300, 80, 100, 100]

        buildJExcel :: Int -> JExcel
        buildJExcel n = defaultJExcel
                      & jExcel_data ?~ [ ["John" , "Doe"    , "$1000"  , "90210"]
                                       , ["Jane" , "Doe"    , "$2000"  , "10010"]
                                       , ["Johan", "Though" , "$3000"  , "20020"]
                                       ]

        toUpperize :: T.Text -> T.Text -> JExcel -> JExcel
        toUpperize password algorithm x = case _jExcel_data x of
            Nothing    -> x
            Just data' -> x & jExcel_data ?~ (map (map id) data')
            --where
                --hashify :: T.Text -> T.Text -> T.Text -> SHA512
                --hashify password algorithm t = hash $ (B8.pack . T.unpack $ t)



{-


-----------
--
--
--
--
--
--

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import                                  Prelude hiding (head)
import           "base"                 Control.Monad (void)
import           "base"                 Control.Monad.IO.Class (liftIO)
import           "base"                 Data.Char (toUpper)
import           "base"                 Data.Monoid ((<>))
import qualified "bytestring"           Data.ByteString.Char8 as B8
import           "containers"           Data.Map.Strict (fromList)
import           "lens"                 Control.Lens
import           "time"                 Data.Time.Clock (getCurrentTime)
import qualified "text"                 Data.Text as T
import           "text"                 Data.Text.Encoding (encodeUtf8)
--import           "memory"               Data.ByteArray
--import           "hashing"              Crypto.Hash
--import           "hashing"              Crypto.Hash.ADT
import           "reflex-dom"           Reflex.Dom
import           "reflex-dom"           Reflex.Dom
--import           "reflex-utils"         Reflex.Utils.Load (script, css, whenLoaded)
import           "reflex-jexcel"        Reflex.JExcel

main :: IO ()
main = do
    print "hi"
-}
