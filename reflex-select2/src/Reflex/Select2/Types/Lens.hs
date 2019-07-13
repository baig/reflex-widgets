{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.Select2.Types.Lens where

import "lens" Control.Lens
import        Reflex.Select2.Types.Types

makeClassy ''Select2Entry
makeClassy ''Select2
