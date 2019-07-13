{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.JExcel.Types.Lens where

import Control.Lens
import Reflex.JExcel.Types.Types

makeClassy ''JExcel
makeClassyPrisms ''JExcelEvent
makeClassyPrisms ''JExcelAlignment
