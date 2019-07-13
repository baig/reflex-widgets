{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Reflex.JExcel.Types.Types where

import "base"  GHC.Generics (Generic)
import "base"  Data.Data (Data)
import "base"  Data.Typeable (Typeable)
import "text"  Data.Text (Text)


-- https://bossanova.uk/jexcel/docs/quick-reference

type ColumnName = Text
type Class      = Text

data JExcel = JExcel
            { _jExcel_data                    :: !(Maybe [[Text]])
            , _jExcel_colHeaders              :: !(Maybe [ColumnName])
            , _jExcel_colWidths               :: !(Maybe [Int])
            , _jExcel_colAlignments           :: !(Maybe [JExcelAlignment])
            , _jExcel_colHeaderClasses        :: !(Maybe [Class])
            , _jExcel_defaultColWidth         :: !(Maybe Int)
            , _jExcel_minSpareRows            :: !(Maybe Int)
            , _jExcel_minSpareCols            :: !(Maybe Int)
            , _jExcel_minDimensions           :: !(Maybe (Int, Int))
            , _jExcel_columnSorting           :: !(Maybe Bool)
            , _jExcel_columnResize            :: !(Maybe Bool)
            , _jExcel_rowDrag                 :: !(Maybe Bool)
            , _jExcel_editable                :: !(Maybe Bool)
            , _jExcel_allowInsertRow          :: !(Maybe Bool)
            , _jExcel_allowManualInsertRow    :: !(Maybe Bool)
            , _jExcel_allowInsertColumn       :: !(Maybe Bool)
            , _jExcel_allowManualInsertColumn :: !(Maybe Bool)
            , _jExcel_allowDeleteRow          :: !(Maybe Bool)
            , _jExcel_allowDeleteColumn       :: !(Maybe Bool)
            , _jExcel_wordWrap                :: !(Maybe Bool)
            , _jExcel_csvFileName             :: !(Maybe Text)
            , _jExcel_selectionCopy           :: !(Maybe Bool)
            } deriving (Show, Data, Typeable, Generic)

type Cell = (Int, Int)

data JExcelEvent = OnLoad
                 | OnBeforeChange Cell Text Text
                 | OnChange Cell Text Text
                 | OnAfterChange
                 | OnInsertRow
                 | OnDeleteRow
                 | OnInsertColumn
                 | OnDeleteColumn
                 | OnSelection Cell Cell
                 | OnSort
                 | OnResize
                 | OnMoveRow
                 | OnFocus
                 | OnBlur
                 deriving (Show, Data, Typeable, Generic)

data JExcelAlignment = JExcelLeft
                     | JExcelRight
                     | JExcelCenter
                     deriving (Show, Enum, Bounded, Data, Typeable, Generic)
