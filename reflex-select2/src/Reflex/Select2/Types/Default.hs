{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.Select2.Types.Default where

import "data-default" Data.Default
import                Reflex.Select2.Types.Types

instance Default Select2Width where
    def = Resolve

instance Default Select2Entry where
    def = Select2Entry "" ""

instance Default Select2 where
    def = Select2
        { _select2_multiple                = Just True
        , _select2_placeholder             = Nothing
        , _select2_width                   = Just "resolve"
        , _select2_dropdownAutoWidth       = Just True
        , _select2_allowClear              = Just True
        , _select2_closeOnSelect           = Just False
        , _select2_minimumInputLength      = Nothing
        , _select2_maximumInputLength      = Nothing
        , _select2_minimumResultsForSearch = Nothing
        , _select2_data                    = Nothing
        , _select2_initialSelection        = Nothing
        , _select2_containerCssClass       = Nothing
        }
