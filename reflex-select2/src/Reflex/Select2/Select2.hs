{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module Reflex.Select2.Select2 ( select2
                              , select2Simple
                              , select2Single
                              , select2Type
                              ) where

import           "base"            Control.Monad (void)
import           "base"            Control.Monad.IO.Class (liftIO)
import           "base"            Data.IORef (IORef, newIORef, writeIORef, readIORef)
import           "base"            Data.Maybe (catMaybes)
import           "text"            Data.Text (Text)
import           "containers"      Data.Map as M (fromList, keys, lookup)
import           "lens"            Control.Lens
import           "reflex-dom"      Reflex.Dom
import           "ghcjs-base"      GHCJS.Types (JSVal)
import           "ghcjs-base"      GHCJS.Marshal (fromJSVal)
import           "ghcjs-dom-jsffi" GHCJS.DOM.Element (IsElement)
import                             Reflex.Select2.Types hiding (select2)
import                             Reflex.Select2.FFI
import qualified                   Reflex.Select2.FFI as FFI (newSelect2, onChange, change)



--
select2Simple :: forall t m. (MonadWidget t m)
              => Text
              -- ^ placeholder
              -> Text
              -- ^ Width style (e.g. "100%" or "200px")
              -> Dynamic t [Text]
              -- ^ Options
              -> Maybe (Dynamic t [Text])
              -- ^ Initially selected options or select all if Nothing
              -> m (Dynamic t [Text])
              -- ^ Selected options
select2Simple placeholder
              widthStyle
              textsD
              mInitialTextsD = select2 (toSelect2 <$> textsD <*> initialTextsD)
    where
        toSelect2 :: [Text] -> [Text] -> Select2
        toSelect2 ts initialTs
            = def
            & select2_placeholder      .~ Just placeholder
            & select2_width            .~ Just widthStyle
            & select2_data             .~ Just (map toEntry ts)
            & select2_initialSelection .~ Just initialTs

        initialTextsD = case mInitialTextsD of
            Just initialTextsD_ -> initialTextsD_
            Nothing             -> textsD


select2Single :: forall t m. (MonadWidget t m)
              => Text
              -- ^ placeholder
              -> Text
              -- ^ Width style (e.g. "100%" or "200px")
              -> Dynamic t [Text]
              -- ^ Options
              -> m (Dynamic t [Text])
              -- ^ Selected options
select2Single placeholder
              widthStyle
              textsD = select2 (toSelect2 <$> textsD)
    where
        toSelect2 :: [Text] -> Select2
        toSelect2 ts
            = def
            & select2_multiple         .~ Nothing
            & select2_placeholder      .~ Just placeholder
            & select2_width            .~ Just widthStyle
            & select2_data             .~ Just (map toEntry ts)

toEntry :: Text -> Select2Entry
toEntry t = Select2Entry
          { _select2Entry_id   = t
          , _select2Entry_text = t
          }

select2Type :: forall a t m. (MonadWidget t m, Show a)
            => Dynamic t [a]
            -- ^ All options
            -> (a -> Text)
            -- ^ Function to show options
            -> Dynamic t Select2
            -- ^ Select2
            -> Dynamic t (Maybe [a])
            -- ^ Initially selected options
            -> m (Dynamic t [a])
            -- ^ Current data
select2Type xsD showFunc select2D mInitialsD = do
    selectedTextsD <- select2 select2WithOptionsD
    return $ cutByKeys <$> optionsMapD <*> selectedTextsD
    where
        optionsMapD = fromList . map (\x -> (showFunc x, x)) <$> xsD
        select2WithOptionsD = addOptions <$> select2D <*> optionsMapD <*> mInitialsD
        addOptions select2_ optionsMap mInitials
            = select2_
            & select2_data .~ Just (def : map toEntry (keys optionsMap))
            & select2_initialSelection .~ emptyToNothing mInitialsText
            where
                textKeys = keys optionsMap
                mInitialsText = (filter (\x -> x `elem` textKeys) . map showFunc <$> mInitials)
                emptyToNothing (Just []) = Nothing
                emptyToNothing x         = x

        cutByKeys map_ ks = catMaybes $ map (\k -> M.lookup k map_) ks

--
select2 :: forall t m. (MonadWidget t m)
        => Dynamic t Select2
        -- ^ Select2
        -> m (Dynamic t [Text])
        -- ^ Current data
select2 select2D = do
    postBuildE <- getPostBuild

    -- select2 element
    (el_, _) <- elAttr' "select" mempty blank

    -- local state
    (select2Ref :: IORef (Maybe Select2Ref)) <- liftIO $ newIORef Nothing

    -- output event + trigger
    (outE, triggerOutE) <- newTriggerEvent

    let inputE = leftmost
               [ tagPromptlyDyn select2D postBuildE
               , updated select2D
               ]

    -- handle input event
    performEvent_ $ ffor inputE $ \select2_ -> liftIO $ onInput (_element_raw el_)
                                                                select2Ref
                                                                triggerOutE
                                                                select2_
    outD <- holdDyn [] outE
    return $ outD

    where
        onInput :: (IsElement el)
                => el
                -- ^ Element
                -> IORef (Maybe Select2Ref)
                -- ^ Select2 ref
                -> ([Text] -> IO ())
                -- ^ Trigger for output event
                -> Select2
                -- ^ data
                -> IO ()
        onInput element_
                select2Ref
                trigger
                select2_ = do
            currentRef_ <- readIORef select2Ref
            case currentRef_ of
                Nothing -> onFirstTime element_ select2Ref trigger select2_
                Just _  -> void $ onNextTime  element_ select2Ref select2_

        onFirstTime :: (IsElement el)
                    => el
                    -- ^ Element
                    -> IORef (Maybe Select2Ref)
                    -- ^ Local state
                    -> ([Text] -> IO ())
                    -- ^ Trigger for output event
                    -> Select2
                    -- ^ data
                    -> IO ()
        onFirstTime element_ select2Ref trigger select2_ = do
            ref <- onNextTime element_ select2Ref select2_
            -- we want to write the trigger only once
            FFI.onChange
                ref
                (onChangeCB trigger (_select2_multiple select2_))
            return ()


        onNextTime :: (IsElement el)
                   => el
                   -- ^ Element
                   -> IORef (Maybe Select2Ref)
                   -- ^ Local state
                   -> Select2
                   -- ^ data
                   -> IO Select2Ref
        onNextTime element_ select2Ref select2_ = do
            -- recreating the object each time as suggested in forums
            ref <- FFI.newSelect2
                element_
                select2_
            writeIORef select2Ref (Just ref)
            FFI.change
                ref
                (_select2_initialSelection select2_)
            return ref


        onChangeCB :: ([Text] -> IO ())
                   -- ^ Trigger
                   -> Maybe Bool
                   -> JSVal
                   -> IO ()
        onChangeCB trigger mIsMultiple jsval_ = do
            ts_ <- case mIsMultiple of
                Just True -> do
                    Just (ts :: [Text]) <- fromJSVal jsval_
                    return ts
                _ -> do
                    (ts :: Maybe Text) <- fromJSVal jsval_
                    return $ catMaybes [ts]
            trigger ts_
            return ()
