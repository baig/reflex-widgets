{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Reflex.JsonEditor.Types.Types where

import "base"       GHC.Generics (Generic)
import "base"       Data.Data (Data)
import "base"       Data.Char (toLower)
import "base"       Data.Typeable (Typeable)
import "aeson"      Data.Aeson as Aeson (Value)

data JsonEditorMode
    = Tree
    | View
    | Form
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)


data JsonEditorOptions
    = JsonEditorOptions
    { _jsonEditorOptions_ace :: !(Maybe Value)
    , _jsonEditorOptions_ajv :: !(Maybe Value)
    , _jsonEditorOptions_escapeUnicode :: !(Maybe Bool)
    , _jsonEditorOptions_sortObjectKeys :: !(Maybe Bool)
    , _jsonEditorOptions_history :: !(Maybe Bool)
    , _jsonEditorOptions_mode    :: !(Maybe JsonEditorMode)
    , _jsonEditorOptions_modes   :: !(Maybe [JsonEditorMode])
    , _jsonEditorOptions_name    :: !(Maybe String)
    , _jsonEditorOptions_schema  :: !(Maybe Value)
    , _jsonEditorOptions_schemaRef :: !(Maybe Value)
    , _jsonEditorOptions_search    :: !(Maybe Bool)
    , _jsonEditorOptions_indentation :: !(Maybe Int)
    , _jsonEditorOptions_theme :: !(Maybe String)
    , _jsonEditorOptions_templaes :: !(Maybe Value)
    , _jsonEditorOptions_autocomplete :: !(Maybe Value)
    , _jsonEditorOptions_mainMenuBar :: !(Maybe Bool)
    , _jsonEditorOptions_navigatoinBar :: !(Maybe Bool)
    , _jsonEditorOptions_statusBar :: !(Maybe Bool)
    , _jsonEditorOptions_colorPicker :: !(Maybe Bool)
    , _jsonEditorOptions_timestampTag :: !(Maybe Bool)
    , _jsonEditorOptions_language :: !(Maybe String)
    , _jsonEditorOptions_languages :: !(Maybe Value)
    , _jsonEditorOptions_modalAnchor :: !(Maybe Value)
    , _jsonEditorOptions_enableSort :: !(Maybe Bool)
    , _jsonEditorOptions_enableTransform :: !(Maybe Bool)
    , _jsonEdtiroOptions_maxVisibleChilds :: !(Maybe Int)
    } deriving (Show, Read, Eq, Data, Typeable, Generic)

fstToLower :: String -> String
fstToLower (x:xs) = (toLower x) : xs
fstToLower _      = ""
