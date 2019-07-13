{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Reflex.ChartJS.ChartJS ( module Reflex.ChartJS.Types
                              , chartjs
                              , simpleAxisFormatter
                              , usdAxisFormatter
                              , percentAxisFormatter
                              , intAxisFormatter
                              , numAxisFormatter
                              ) where
import "base"             Control.Monad.IO.Class (liftIO)
import "base"             Data.IORef (IORef, newIORef, writeIORef, readIORef)
import "containers"       Data.Map (Map)
import "text"             Data.Text (Text)
import "text"             Data.Text.Encoding (encodeUtf8)
{-import "formattable"      Formattable.NumFormat (NumFormat, formatNum, usdFmt, percentFmt, intFmt, numFmt)-}
import "aeson"            Data.Aeson as Aeson (decodeStrict)
import "jsaddle"          Language.Javascript.JSaddle -- (JSVal) --  GHCJS.Types (JSVal)
{-import "ghcjs-base"       GHCJS.Marshal (toJSVal, fromJSVal)-}
import                    GHCJS.DOM.Element -- (IsElement)
import "reflex"           Reflex
import "reflex-dom"       Reflex.Dom hiding (Value)
import                    Reflex.ChartJS.FFI
import                    Reflex.ChartJS.Types


{-simpleAxisFormatter :: NumFormat -> AxisFormatter-}
simpleAxisFormatter format inputJsval = do
    undefined
    {-(number' :: Maybe Double) <- fromJSVal inputJsval-}
    {-case number' of-}
        {-Nothing -> return inputJsval-}
        {-Just number -> do-}
            {-undefined-}
            {-toJSVal . formatNum format $ number-}


usdAxisFormatter :: AxisFormatter
usdAxisFormatter = undefined -- simpleAxisFormatter usdFmt

percentAxisFormatter :: AxisFormatter
percentAxisFormatter = undefined -- simpleAxisFormatter percentFmt

intAxisFormatter :: AxisFormatter
intAxisFormatter = undefined -- simpleAxisFormatter intFmt

numAxisFormatter :: AxisFormatter
numAxisFormatter = undefined -- simpleAxisFormatter numFmt

chartjs :: forall t m. (MonadWidget t m)
        => ChartJsCallbacks
        -- ^ ChartJs callbacks
        -> Dynamic t (Map Text Text)
        -- ^ Dynamic attributes
        -> Dynamic t ChartJs
        -- ^ Dynamic input chartjs data
        -> m (Event t ChartJsSelection)
        --    ^ Dynamic output event
chartjs callbacks
        attrsD
        chartJsD = do
    -- HTML element
    (element_, _) <- elDynAttr "div" attrsD $ el' "canvas" blank

    -- local state
    (chartJsRef :: IORef (Maybe ChartJsRef)) <- liftIO $ newIORef Nothing

    -- input event
    (postBuildTaggedE :: Event t ChartJs) <- tagPromptlyDyn chartJsD <$> getPostBuild
    let inputE = leftmost
               [ postBuildTaggedE
               , updated chartJsD
               ]

    -- output event + trigger
    (outE :: Event t ChartJsSelection, triggerOutE) <- newTriggerEvent

    -- handle input event
    ctxRef <- askJSM
    performEvent_ $ ffor inputE $ \chartJs_ -> flip runJSM ctxRef $ onInputChange (_element_raw element_)
                                                                      chartJsRef
                                                                      triggerOutE
                                                                      chartJs_
    return outE

    where
        onInputChange :: (IsElement el)
                      => el
                      -- ^ Element
                      -> IORef (Maybe ChartJsRef)
                      -- ^ Local state
                      -> (ChartJsSelection -> IO ())
                      -- ^ Trigger for output event
                      -> ChartJs
                      -- ^ Chart data
                      -> JSM ()
        onInputChange element_ chartJsRef trigger chartJs_ = do
            currentRef_ <- liftIO $ readIORef chartJsRef
            case currentRef_ of
                Nothing  -> onFirstTime element_ chartJsRef trigger chartJs_
                Just ref -> onNextTime  ref                         chartJs_


        onFirstTime :: (IsElement el)
                    => el
                    -- ^ Element
                    -> IORef (Maybe ChartJsRef)
                    -- ^ Local state
                    -> (ChartJsSelection -> IO ())
                    -- ^ Trigger for output event
                    -> ChartJs
                    -- ^ Chart data
                    -> JSM ()
        onFirstTime element_ chartJsRef trigger chartJs_ = do
            liftIO $ print ("onFirstTime" :: String)
            ref <- newChart element_
                            chartJs_
                            callbacks
            liftIO $ writeIORef chartJsRef (Just ref)

            registerOnClick element_
                            ref
                            (onClick trigger)

            return ()


        onNextTime :: ChartJsRef
                   -- ^ Current value of local state
                   -> ChartJs
                   -- ^ Chart data
                   -> JSM ()
        onNextTime ref chartJs_ = do
            update ref chartJs_
            return ()


        onClick :: (ChartJsSelection -> IO ())
                -> ChartJsSelection
                -> JSM ()
        onClick trigger event = do
            liftIO $ trigger event
            return ()

