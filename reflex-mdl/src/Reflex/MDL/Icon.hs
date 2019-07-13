{-# LANGUAGE PackageImports #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Reflex.MDL.Icon where

import "base"         Data.Monoid ((<>))
import "text"         Data.Text (pack)
import "reflex-dom"   Reflex.Dom

data Icon = IconEdit
          | IconDelete
          | IconArchive
          | IconHome
          | IconGraphs
          | IconDownload
          | IconRuns
          | IconList
          | IconConfig
          | IconRunning
          | IconError
          | IconDone
          | IconUploaded
          | IconUpload
          | IconPlay
          | IconPeople
          | IconQueued
          | IconTrendingUp
          | IconTrendingDown
          | IconLightbulbOutLine
          | IconDashboard
          | IconFindOnPage
          | IconTakeoff
          | IconClear
          | IconExpandMore
          | IconExpandLess
          | IconWork
          | IconAccountCircle
          | IconPublic
          | IconAttachMoney
          | IconSwapHoriz
          | IconPoll
          | IconShoppingCart
          | IconNavigateNext
          | IconNavigateBefore
          | IconSave
          | IconSettings
          deriving (Eq)

instance Show Icon where
    show IconEdit             = "edit"
    show IconDelete           = "delete"
    show IconArchive          = "archive"
    show IconHome             = "home"
    show IconGraphs           = "trending_up"
    show IconTrendingUp       = "trending_up"
    show IconTrendingDown     = "trending_down"
    show IconDownload         = "file_download"
    show IconRuns             = "playlist_play"
    show IconList             = "list"
    show IconConfig           = "view_list"
    show IconRunning          = "directions_run"
    show IconQueued           = "airline_seat_recline_extra"
    show IconError            = "error"
    show IconDone             = "done"
    show IconUploaded         = "cloud_done"
    show IconUpload           = "cloud_upload"
    show IconPlay             = "play_arrow"
    show IconPeople           = "people"
    show IconFindOnPage       = "find_in_page"
    show IconTakeoff          = "flight_takeoff"
    show IconDashboard        = "dashboard"
    show IconLightbulbOutLine = "lightbulb_outline"
    show IconClear            = "clear"
    show IconExpandMore       = "expand_more"
    show IconExpandLess       = "expand_less"
    show IconWork             = "work"
    show IconAccountCircle    = "account_circle"
    show IconPublic           = "public"
    show IconAttachMoney      = "attach_money"
    show IconSwapHoriz        = "swap_horiz"
    show IconShoppingCart     = "shopping_cart"
    show IconPoll             = "poll"
    show IconNavigateNext     = "navigate_next"
    show IconNavigateBefore   = "navigate_before"
    show IconSave             = "save"
    show IconSettings         = "settings"

mdlIcon :: MonadWidget t m
        => Bool
        -- ^ with space for label
        -> Dynamic t Icon
        -- ^ icon
        -> m (El t, ())
mdlIcon withSpace iconD = do
    elAttr' "i" ( "class" =: "material-icons atidot-icon"
               <> "role"  =: "presentation"
               <> if withSpace
                  then "style" =: "vertical-align: middle; margin-right: 10px;"
                  else mempty
                ) $ dynText iconTextD
    where
        iconTextD = pack . show <$> iconD
