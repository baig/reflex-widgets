{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Reflex.Select2.Types.Types where

import "base" GHC.Generics (Generic)
import "base" Data.Data (Data)
import "base" Data.Typeable (Typeable)
import "text" Data.Text (Text)

data Select2Width = Element
                  | Style
                  | Resolve
                  deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)


data Select2Entry = Select2Entry
                  { _select2Entry_id   :: !Text
                  , _select2Entry_text :: !Text
                  } deriving (Show, Read, Eq, Data, Typeable, Generic)


data Select2 = Select2
             { _select2_multiple                :: !(Maybe Bool)
             , _select2_placeholder             :: !(Maybe Text)
             , _select2_width                   :: !(Maybe Text)
             , _select2_dropdownAutoWidth       :: !(Maybe Bool)
             , _select2_allowClear              :: !(Maybe Bool)
             , _select2_minimumInputLength      :: !(Maybe Int)
             , _select2_maximumInputLength      :: !(Maybe Int)
             , _select2_minimumResultsForSearch :: !(Maybe Int)
             , _select2_closeOnSelect           :: !(Maybe Bool)
             , _select2_data                    :: !(Maybe [Select2Entry])
             , _select2_initialSelection        :: !(Maybe [Text])
             , _select2_containerCssClass       :: !(Maybe Text)
             } deriving (Show, Read, Eq, Data, Typeable, Generic)
