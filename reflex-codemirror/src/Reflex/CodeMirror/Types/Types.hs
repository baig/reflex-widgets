{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Reflex.CodeMirror.Types.Types where

import "base"  GHC.Generics (Generic)
import "base"  Data.Data (Data)
import "base"  Data.Typeable (Typeable)
import "text"  Data.Text (Text)


data Configuration
    = Configuration
    { _configuration_value          :: !(Maybe Text)
    , _configuration_mode           :: !(Maybe Text)
    , _configuration_lineSeparator  :: !(Maybe Text)
    , _configuration_theme          :: !(Maybe Text)
    , _configuration_indentUnit     :: !(Maybe Int)
    , _configuraiton_smartIndent    :: !(Maybe Bool)
    , _configuration_tabSize        :: !(Maybe Int)
    , _configuration_indentWithTabs :: !(Maybe Bool)
    , _configuration_electricChars  :: !(Maybe Bool)
    --
    -- TODO: add more
    --
    , _configuration_lineWrapping            :: !(Maybe Bool)
    , _configuration_lineNumbers             :: !(Maybe Bool)
    , _configuration_firstLineNumber         :: !(Maybe Int)
    , _configuration_fixedGutter             :: !(Maybe Bool)
    , _configuration_scrollbarStyle          :: !(Maybe Text)
    , _configuration_inputStyle              :: !(Maybe Text)
    , _configuration_readOnly                :: !(Maybe Bool)
    , _configuration_showCursorWhenSelecting :: !(Maybe Bool)
    , _configuration_lineWiseCopyCut         :: !(Maybe Bool)
    , _configuration_pasteLinePerSelection   :: !(Maybe Bool)
    , _configuration_undoDepth               :: !(Maybe Int)
    , _configuration_historyEventDelay       :: !(Maybe Int)
    , _configuration_tabIndex                :: !(Maybe Int)
    , _configuration_autoFocus               :: !(Maybe Bool)
    , _configuration_autoRefresh             :: !(Maybe Bool)
    --
    , _configuration_pollInterval :: !(Maybe Int) -- Ms
    --
    } deriving (Show, Read, Eq, Data, Typeable, Generic)
