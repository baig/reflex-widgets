{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.CodeMirror.Types.Default where

import "data-default" Data.Default
import "derive"       Data.DeriveTH (derive, makeDefault)
import                Reflex.CodeMirror.Types.Types

$(derive makeDefault ''Configuration)
