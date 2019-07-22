{-# LANGUAGE PackageImports #-}
module Reflex.FileAPI.FFI where

import                       Prelude hiding ((!!), readFile)
import           "base"      Control.Monad.IO.Class (liftIO)
import           "lens"      Control.Lens hiding (element, (#))
import           "stm"       Control.Concurrent.STM
import           "stm-chans" Control.Concurrent.STM.TBMQueue
import           "text"      Data.Text (Text)
--import qualified "text"      Data.Text as T
import           "jsaddle"   Language.Javascript.JSaddle
import                       GHCJS.DOM.Element (IsElement, toElement, unElement)


readFileW :: ( IsElement element
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
         -> Maybe Text
         -- ^ File name
         -> Maybe Int
         -- ^ File size
         -> Maybe Text
         -- ^ Text read from file
         -> Bool
         -- ^ Is done working
         -> JSM ()
readFileW q element start step
          _ mFileSize mResult isDone
    = do
    if isDone
    then do
        liftIO $ atomically $ closeTBMQueue q
        return ()
    else do
        case mFileSize of
            -- first run
            Nothing -> do
                readSlice element
                          (readFileW q element 0 step)
                          start
                          step
            -- next runs
            Just _ -> do
                case mResult of
                    -- done
                    Nothing -> do
                        liftIO $ atomically $ closeTBMQueue q
                        return ()

                    -- write to queue and continue
                    Just result -> do
                        liftIO $ atomically $ writeTBMQueue q result
                        readSlice element
                                  (readFileW q element (start + step) step)
                                  start
                                  step
                        return ()



readSlice :: ( IsElement element
             )
          => element
          -- ^ Element
          -> (Maybe Text -> Maybe Int -> Maybe Text -> Bool -> JSM ())
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

    -- if finished reading everything
    if start >= size - 1
    then do
        callback Nothing
                 Nothing
                 Nothing
                 True
        return ()
    else do
        reader <- new (jsg "FileReader") ()
        reader ^. jss "onloadend" (fun $ \_ _ [evt] -> do
            let fileReaderDone = 2
            readyState <- valToNumber =<< evt ^. js "target" ^. js "readyState"
            if readyState == fileReaderDone
            then do
                name <- valToText =<< file ^. js "name"
                text <- valToText =<< evt ^. js "target" ^. js "result"
                callback (Just name)
                         (Just size)
                         (Just text)
                         False
            else do
                return ()
            )
        blob <- file ^. js2 "slice" start (min (start + step) size)
        _ <- reader ^. js1 "readAsBinaryString" blob
        return ()
