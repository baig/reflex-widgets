{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.JsonEditor.Types.Default where


import "data-default" Data.Default
import "aeson"        Data.Aeson as Aeson (Value(..))
import "derive"       Data.DeriveTH (derive, makeDefault)
import                Reflex.JsonEditor.Types.Types

instance Default Value where
    def = Null

$(derive makeDefault ''JsonEditorMode)
$(derive makeDefault ''JsonEditorOptions)
