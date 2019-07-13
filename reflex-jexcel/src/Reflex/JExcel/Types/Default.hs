{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.JExcel.Types.Default where

import "data-default" Data.Default
import                Reflex.JExcel.Types.Types

instance Default JExcel where
    def = JExcel
        { _jExcel_data                    = Nothing
        , _jExcel_colHeaders              = Nothing
        , _jExcel_colWidths               = Nothing
        , _jExcel_colAlignments           = Nothing
        , _jExcel_colHeaderClasses        = Nothing
        , _jExcel_defaultColWidth         = Nothing
        , _jExcel_minSpareRows            = Nothing
        , _jExcel_minSpareCols            = Nothing
        , _jExcel_minDimensions           = Nothing
        , _jExcel_columnSorting           = Nothing
        , _jExcel_columnResize            = Nothing
        , _jExcel_rowDrag                 = Nothing
        , _jExcel_editable                = Nothing
        , _jExcel_allowInsertRow          = Nothing
        , _jExcel_allowManualInsertRow    = Nothing
        , _jExcel_allowInsertColumn       = Nothing
        , _jExcel_allowManualInsertColumn = Nothing
        , _jExcel_allowDeleteRow          = Nothing
        , _jExcel_allowDeleteColumn       = Nothing
        , _jExcel_wordWrap                = Nothing
        , _jExcel_csvFileName             = Nothing
        , _jExcel_selectionCopy           = Nothing
        }

instance Default JExcelEvent where
    def = OnLoad

instance Default JExcelAlignment where
    def = JExcelLeft
