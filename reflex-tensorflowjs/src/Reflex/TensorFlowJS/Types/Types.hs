{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Reflex.TensorFlowJS.Types.Types where

import "base"       GHC.Generics (Generic)
import "base"       Data.Data (Data)
import "base"       Data.Char (toLower)
import "base"       Data.Typeable (Typeable)
import "aeson"      Data.Aeson as Aeson (Value)


fstToLower :: String -> String
fstToLower (x:xs) = (toLower x) : xs
fstToLower _      = ""
