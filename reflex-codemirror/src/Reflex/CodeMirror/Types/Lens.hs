{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.CodeMirror.Types.Lens where

import Control.Lens
import Reflex.CodeMirror.Types.Types

makeClassy ''Configuration

