{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.TensorFlowJS.Types.Default where


import "data-default" Data.Default
import "aeson"        Data.Aeson as Aeson (Value(..))
import "derive"       Data.DeriveTH (derive, makeDefault)
import                Reflex.TensorFlowJS.Types.Types

instance Default Value where
    def = Null
