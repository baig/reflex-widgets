{-# LANGUAGE PackageImports #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Reflex.JExcel.FFI where

import             Prelude hiding ((!!))
import "base"      Data.Maybe (fromJust)
import "base"      Control.Monad.IO.Class (liftIO)
import "lens"      Control.Lens hiding (element, (#))
import "aeson"     Data.Aeson (toJSON, Value(..), decodeStrict, encode)
import "aeson"     Data.Aeson as Aeson (decodeStrict)
import "text"      Data.Text (Text, split, unpack)
import qualified "text"      Data.Text as T (length)
import "text"      Data.Text.Encoding (encodeUtf8)
import "jsaddle"   Language.Javascript.JSaddle
import "jsaddle"  Language.Javascript.JSaddle.Value
import             GHCJS.DOM.Element (IsElement, toElement, unElement)
import             Reflex.JExcel.Types


type ColumnNumber = Int
type RowNumber = Int
type CellIdent = Text


class JExcelHandlers a where
    onload :: a -> JExcelEvent -> JSM ()
    onload _ _ = return ()

    onbeforechange :: a -> JExcelEvent -> JSM ()
    onbeforechange _ _ = return ()

    onchange :: a -> JExcelEvent -> JSM ()
    onchange _ _ = return ()

    onafterchange :: a -> JExcelEvent -> JSM ()
    onafterchange _ _ = return ()

    oninsertrow :: a -> JExcelEvent -> JSM ()
    oninsertrow _ _ = return ()

    ondeleterow :: a -> JExcelEvent -> JSM ()
    ondeleterow _ _ = return ()

    oninsertcolumn :: a -> JExcelEvent -> JSM ()
    oninsertcolumn _ _ = return ()

    ondeletecolumn :: a -> JExcelEvent -> JSM ()
    ondeletecolumn _ _ = return ()

    onselection :: a -> JExcelEvent -> JSM ()
    onselection _ _ = return ()

    onsort :: a -> JExcelEvent -> JSM ()
    onsort _ _ = return ()

    onresize :: a -> JExcelEvent -> JSM ()
    onresize _ _ = return ()

    onmoverow :: a -> JExcelEvent -> JSM ()
    onmoverow _ _ = return ()

    onfocus :: a -> JExcelEvent -> JSM ()
    onfocus _ _ = return ()

    onblur :: a -> JExcelEvent -> JSM ()
    onblur _ _ = return ()


--withHandler :: (ToJSString a0, MakeObject s) => s -> a0 -> (JExcelEvent -> JSM ()) -> JSM ()
--withHandler config name callback = do
    --_ <- config ^. jss name (fun $ \_ _ evts -> do
        --case length kkkk
        --let y = head evts
        --(x :: Text) <- valToText y
        --print x
        ----case head texts of
            ----Nothing -> return ()
            ----Just text -> do
                ----print text
                ------callback event -- value
        --w <- jsg "console"
        --_ <- w ^. js1 "log" evts
        ----_ <- w ^. js1 "log" name
        --return ())
    --return ()


jsvalToCell :: JSVal -> JSM Cell
jsvalToCell val' = do
    tmp  <- val' !! 0
    id'  <- tmp ^. js "id"
    text <- valToText id'
    liftIO $ print text
    let (column : row : _) = split (== '-') text
    return (read . unpack $ column, read . unpack $ row)



newJExcel :: ( IsElement element
             , JExcelHandlers handlers
             )
          => element
          -- ^ Element
          -> JExcel
          -- ^ Configuration
          -> handlers
          -- ^ JExcel handlers
          -> JSM ()
newJExcel element
          jexcelConfig
          handlers
    = do
    let js_element' = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    liftIO $ print (toJSON $ jexcelConfig)
    js_config <- toJSVal . toJSON $ jexcelConfig
    js_config ^. jss "onload" (fun $ \_ _ _ -> onload handlers $ OnLoad)
    js_config ^. jss "onbeforechange" (fun $ \_ _ [_, js_cell, js_before, js_after] -> do
        cell <- jsvalToCell js_cell
        before <- valToText js_before
        after  <- valToText js_after
        onbeforechange handlers $ OnBeforeChange cell before after)
    js_config ^.jss "onchange" (fun $ \_ _ [_, js_cell, js_after, js_before] -> do
        cell <- jsvalToCell js_cell
        before <- valToText js_before
        after  <- valToText js_after
        onchange handlers $ OnChange cell after before)
    js_config ^. jss "onafterchange" (fun $ \_ _ _ -> onafterchange handlers $ OnAfterChange)
    js_config ^. jss "oninsertrow" (fun $ \_ _ _ -> oninsertrow handlers $ OnInsertRow)
    js_config ^. jss "ondeleterow" (fun $ \_ _ _ -> ondeleterow handlers $ OnDeleteRow)
    js_config ^. jss "oninsertcolumn" (fun $ \_ _ _ -> oninsertcolumn handlers $ OnInsertColumn)
    js_config ^. jss "ondeletecolumn" (fun $ \_ _ _ -> ondeletecolumn handlers $ OnDeleteColumn)
    js_config ^. jss "onselection" (fun $ \_ _ [_, cellTopLeft, cellBottomRight, _] -> do
        topLeft     <- jsvalToCell cellTopLeft
        bottomRight <- jsvalToCell cellBottomRight
        onselection handlers $ OnSelection topLeft bottomRight)
    js_config ^. jss "onsort" (fun $ \_ _ _ -> onsort handlers $ OnSort)
    js_config ^. jss "onresize" (fun $ \_ _ _ -> onresize handlers $ OnResize)
    js_config ^. jss "onmoverow" (fun $ \_ _ _ -> onmoverow handlers $ OnMoveRow)
    js_config ^. jss "onfocus" (fun $ \_ _ _ -> onfocus handlers $ OnFocus)
    js_config ^. jss "onblur" (fun $ \_ _ _ -> onblur handlers $ OnBlur)

    _ <- js_element ^. js1 "jexcel" js_config
    return ()

getData :: (IsElement element)
        => element
        -- ^ Element
        -> Bool
        -- ^ Get only highlighted
        -> JSM JSVal
getData element
        onlyHighlighted
    = do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    result <- js_element ^. js2 "jexcel" "getData" onlyHighlighted
    return result


setData :: (IsElement element)
        => element
        -- ^ Element
        -> Maybe [[Text]]
        -- ^ Optional: new JSON data
        -> Bool
        -- ^ Ignore Spare
        -> JSM ()
setData element
        mNewData
        ignoreSpare
    = do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    case mNewData of
        Nothing -> return ()
        Just newData -> do
            let js_newData  = toJSON $ newData
            _ <- js_element ^. js3 "jexcel" "setData" js_newData ignoreSpare
            return ()
    return ()


deleteColumn :: (IsElement element)
             => element
             -- ^ Element
             -> ColumnNumber
             -- ^ Column number
             -> JSM ()
deleteColumn element
             columnNumber
    = do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    _ <- js_element ^. js2 "jexcel" "deleteColumn" columnNumber
    return ()


insertRow :: (IsElement element)
          => element
          -- ^ Element
          -> RowNumber
          -- ^ Row number
          -> JSM ()
insertRow element
          rowNumber
    = do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    _ <- js_element ^. js2 "jexcel" "insertRow" rowNumber
    return ()


deleteRow :: (IsElement element)
          => element
          -- ^ Element
          -> RowNumber
          -- ^ Row number
          -> JSM ()
deleteRow element
          rowNumber
    = do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    _ <- js_element ^. js2 "jexcel" "deleteRow" rowNumber
    return ()


getHeader :: (IsElement element)
          => element
          -- ^ Element
          -> ColumnNumber
          -- ^ Column number
          -> JSM JSVal
getHeader element
          columnNumber
    =  do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    result <- js_element ^. js2 "jexcel" "getHeader" columnNumber
    return result


setHeader :: (IsElement element)
          => element
          -- ^ Element
          -> ColumnNumber
          -- ^ Column number
          -> Text
          -- ^ Coliumn title
          -> JSM ()
setHeader element
          columnNumber
          columnTitle
    = do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    _ <- js_element ^. js3 "jexcel" "setHeader" columnNumber columnTitle
    return ()


getWidth :: (IsElement element)
         => element
         -- ^ Element
         -> ColumnNumber
         -- ^ Column number
         -> JSM JSVal
getWidth element
         columnNumber
    = do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    result <- js_element ^. js2 "jexcel" "getWidth" columnNumber
    return result

setWidth :: (IsElement element)
         => element
         -- ^ Element
         -> ColumnNumber
         -- ^ Column number
         -> Int
         -- ^ New column width
         -> JSM ()
setWidth element
         columnNumber
         newColumnWidth
    = do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    _ <- js_element ^. js3 "jexcel" "setWidth" columnNumber newColumnWidth
    return ()


orderBy :: (IsElement element)
        => element
        -- ^ Element
        -> ColumnNumber
        -- ^ Column number
        -> Int
        -- ^ Sort type
        -> JSM ()
orderBy element
        columnNumber
        sortType
    = do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    _ <- js_element ^. js3 "jexcel" "orderBy" columnNumber sortType
    return ()


getValue :: (IsElement element)
         => element
         -- ^ Element
         -> CellIdent
         -- ^ Cell ID
         -> JSM JSVal
getValue element
         cellIdent
    = do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    result <- js_element ^. js2 "jexcel" "getValue" cellIdent
    return result


setValue :: (IsElement element)
         => element
         -- ^ Element
         -> CellIdent
         -- ^ Cell ID
         -> JSVal
         -- ^ Value
         -> JSM ()
setValue element
         cellIdent
         value
    = do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    _ <- js_element ^. js3 "jexcel" "setValue" cellIdent value
    return ()


updateSelection :: (IsElement element)
                => element
                -- ^ Element
                -> CellIdent
                -- ^ Start Cell
                -> CellIdent
                -- ^ End Cell
                -> Bool
                -- ^ Ignore Events
                -> JSM ()
updateSelection element
                startCell
                endCell
                ignoreEvents
    = do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    _ <- js_element ^. js4 "jexcel" "updateSelection" startCell endCell ignoreEvents
    return ()


download :: (IsElement element)
         => element
         -- ^ Element
         -> JSM ()
download element
    = do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    _ <- js_element ^. js1 "jexcel" "download"
    return ()


getConfig :: (IsElement element)
          => element
          -- ^ Element
          -> Text
          -- ^ key
          -> JSM JSVal
getConfig element
          key
    = do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    result <- js_element ^. js2 "jexcel" "getConfig" key
    return result


setConfig :: (IsElement element)
          => element
          -- ^ Element
          -> Text
          -- ^ key
          -> JSVal
          -- ^ value
          -> JSM ()
setConfig element
          key
          value
    = do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    _ <- js_element ^. js3 "jexcel" "setconfig" key value
    return ()


getStyle :: (IsElement element)
         => element
         -- ^ Element
         -> Maybe CellIdent
         -- ^ Cell or entire table
         -> JSM JSVal
getStyle element
         mCellIdent
    = do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    result <- js_element ^. js2 "jexcel" "getStyle" mCellIdent
    return result


setStyle :: (IsElement element)
         => element
         -- ^ Element
         -> JSVal
         -- ^ CSS Key,Value -- Map CellIdent JSVal
         -> JSM ()
setStyle element
         styles
    = do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    _ <- js_element ^. js2 "jexcel" "setSTyle" styles
    return ()


getComments :: (IsElement element)
            => element
            -- ^ Element
            -> Maybe CellIdent
            -- ^ Cell Ident or table
            -> JSM JSVal
getComments element
            mCellIdent
    = do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    result <- js_element ^. js2 "jexcel" "getComments" mCellIdent
    return result


setComments :: (IsElement element)
            => element
            -- ^ Element
            -> CellIdent
            -- ^ Cell Ident
            -> Text
            -- ^ Comments
            -> JSM ()
setComments element
            cellIdent
            comment
    = do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    _ <- js_element ^. js3 "jexcel" "setComments" cellIdent comment
    return ()


getMeta :: (IsElement element)
        => element
        -- ^ Element
        -> Maybe CellIdent
        -- ^ Cell Ident or Table
        -> JSM JSVal
getMeta element
        mCellIdent
    = do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    result <- js_element ^. js2 "jexcel" "getMeta" mCellIdent
    return result


setMeta :: (IsElement element)
        => element
        -- ^ Element
        -> JSVal
        -- ^ metas
        -> JSM JSVal
setMeta element
        metas
    = do
    let js_element'  = unElement . toElement  $ element
    js_element <- jsgf "jQuery" js_element'
    result <- js_element ^. js2 "jexcel" "setMeta" metas
    return result

