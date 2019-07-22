{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Reflex.JExcel.JExcel where

import "base"             Control.Monad.IO.Class (liftIO)
import "base"             Data.IORef (IORef, newIORef, writeIORef, readIORef)
import "aeson"            Data.Aeson (Value(..))
import "text"             Data.Text (Text)
import "jsaddle"          Language.Javascript.JSaddle
import "reflex"           Reflex
import "reflex-dom"       Reflex.Dom hiding (Value)
import                    Reflex.JExcel.Types
import                    Reflex.JExcel.FFI


data (JExcelHandlers handlers) => JExcelInput t handlers
    = JExcelInput
    { _jexcelInput_htmlId   :: Text
    , _jexcelInput_jexcel   :: Dynamic t JExcel
    }

data JExcelOutput t
    = JExcelOutput
    { _jexcelOutput_value :: Dynamic t Value
    , _jexcelOutput_event :: Event t JExcelEvent
    }


data JExcelReflex
    = JExcelReflex
    { _jexcelReflex_trigger :: (JExcelEvent -> IO ())
    }


instance JExcelHandlers JExcelReflex where
    onload self event         = liftIO $ (_jexcelReflex_trigger self) event
    onbeforechange self event = liftIO $ (_jexcelReflex_trigger self) event
    onchange self event       = liftIO $ (_jexcelReflex_trigger self) event
    onafterchange self event  = liftIO $ (_jexcelReflex_trigger self) event
    oninsertrow self event    = liftIO $ (_jexcelReflex_trigger self) event
    ondeleterow self event    = liftIO $ (_jexcelReflex_trigger self) event
    oninsertcolumn self event = liftIO $ (_jexcelReflex_trigger self) event
    ondeletecolumn self event = liftIO $ (_jexcelReflex_trigger self) event
    onselection self event    = liftIO $ (_jexcelReflex_trigger self) event
    onsort self event         = liftIO $ (_jexcelReflex_trigger self) event
    onresize self event       = liftIO $ (_jexcelReflex_trigger self) event
    onmoverow self event      = liftIO $ (_jexcelReflex_trigger self) event
    onfocus self event        = liftIO $ (_jexcelReflex_trigger self) event
    onblur self event         = liftIO $ (_jexcelReflex_trigger self) event


jexcel :: forall t m. (MonadWidget t m)
       => JExcelInput t JExcelReflex
       -- ^ Inputs
       -> m (JExcelOutput t)
jexcel (JExcelInput htmlId jexcelD) = do

    -- create element
    let attrsD = ("id" =:) <$> constDyn htmlId
    (element', _) <- elDynAttr' "div" attrsD blank

    -- input event
    (postBuildTaggedE' :: Event t JExcel) <- tagPromptlyDyn jexcelD <$> getPostBuild

    -- TODO: HACK FIX THIS!!
    postBuildTaggedE <- delay 0.1 postBuildTaggedE'

    let inputE = leftmost
               [ -- postBuildTaggedE
                 updated jexcelD
               ]

    -- output event + trigger
    (outE :: Event t JExcelEvent, trigger) <- newTriggerEvent

    -- local state
    (ref :: IORef (Maybe Bool)) <- liftIO $ newIORef Nothing

    -- handle input event
    ctxRef <- askJSM
    performEvent_ $ ffor inputE $ \(config) -> flip runJSM ctxRef $ do
        mRef <- liftIO $ readIORef ref
        case mRef of
            -- First time
            Nothing -> do
                newJExcel (_element_raw element')
                          config
                          (JExcelReflex trigger)
                liftIO $ writeIORef ref (Just True)

            -- Next times
            Just _ -> do
                setData (_element_raw element')
                        (_jExcel_data config)
                        True
                return ()

    return $ JExcelOutput (constDyn Null)
                          outE
