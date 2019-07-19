{-# LANGUAGE PackageImports #-}
module Reflex.FileAPI.FileAPI where

import                       Prelude hiding ((!!), readFile)
import           "base"      Control.Monad.IO.Class (liftIO)
import           "lens"      Control.Lens hiding (element, (#))
import           "stm"       Control.Concurrent.STM
import           "stm-chans" Control.Concurrent.STM.TBMQueue
import           "text"      Data.Text (Text)
import qualified "text"      Data.Text as T
import           "jsaddle"   Language.Javascript.JSaddle
import                       GHCJS.DOM.Element (IsElement, toElement, unElement)

readFile :: ( IsElement element
            )
         => TBMQueue Text
         -- ^ Output Queue
         -> element
         -- ^ Element
         -> Int
         -- ^ Start
         -> Int
         -- ^ Step
         ---------
         -> Text
         -- ^ File name
         -> Int
         -- ^ File size
         -> Text
         -- ^ Text read from file
         -> JSM ()
readFile q element start step
         _ _ result
    = do
    if T.length result == 0
    then do
        liftIO $ atomically $ closeTBMQueue q
        return ()
    else do
        liftIO $ atomically $ writeTBMQueue q result
        readSlice element
                  (readFile q element (start + step) step)
                  start
                  step
        return ()


readSlice :: ( IsElement element
             )
          => element
          -- ^ Element
          -> (Text -> Int -> Text -> JSM ())
          -- ^ Callback
          -> Int
          -- ^ Start
          -> Int
          -- ^ Step
          -> JSM ()
readSlice element
          callback
          start
          step
    = do
    let js_element = unElement . toElement  $ element
    files  <- js_element ^. js "files"
    file   <- files !! 0
    size'  <- valToNumber =<< file ^. js "size"
    let size = floor size'
    reader <- new (jsg "FileReader") ()
    reader ^. jss "onloadend" (fun $ \_ _ [evt] -> do
        name <- valToText =<< file ^. js "name"
        text <- valToText =<< evt ^. js "target" ^. js "result"
        callback name size text
        )
    blob <- file ^. js2 "slice" start (min (start + step) size)
    _ <- reader ^. js1 "readAsBinaryString" blob
    return ()


