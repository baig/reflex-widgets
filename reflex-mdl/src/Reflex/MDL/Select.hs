{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE MonoLocalBinds #-}
module Reflex.MDL.Select where

import           "base"       Data.Monoid ((<>))
import           "base"       Data.List (findIndex)
import qualified "containers" Data.Map as Map
import qualified "text"       Data.Text as T
import           "reflex-dom" Reflex.Dom
import           "lens"       Control.Lens
{-import                        Reflex.MDL.Common (materialize')-}

mdlSelect :: forall a t m. (MonadWidget t m, Show a, Eq a)
          => Dynamic t T.Text
          -- ^ HTML id
          -> T.Text
          -- ^ Selection name
          -> Dynamic t [a]
          -- ^ value
          -> Dynamic t (Maybe a)
          -- ^ Pre-selected value
          -> m (Event t T.Text)
mdlSelect htmlIdD
          selectionName
          valuesD
          mDefaultValueD = -- TODO MDL select doesn't work
    divClass (T.intercalate " " defaultSelectClasses)  $ do
        let attrD = ( (("class" =: "mdl-textfield__input") <>)
                      . ("id" =:)
                      )
                  <$> htmlIdD
        textResult <- textInput $ def
                                & textInputConfig_inputType    .~ "text"
                                & textInputConfig_initialValue .~ ""
                                & textInputConfig_attributes   .~ attrD
                                                                
        {-firstVal <- fromMaybe $ sample . current $ -}
        _ <- textInput $ def
                       & textInputConfig_inputType    .~ "hidden"
                       & textInputConfig_initialValue .~ ""
                       & textInputConfig_attributes   .~ (("name" =:) <$> htmlIdD)

        let iconAttrsD = "class" =: "mdl-icon-toggle__label material-icons" 
        _ <- elAttr "i"
                    iconAttrsD
                    (text "keyboard_arrow_down") 
        
        let labelAttrsD = ("class" =: "mdl-textfield__label" <>)
                      <$> forDAttr

        _ <- elDynAttr' "label"
                       labelAttrsD
                       (text selectionName)
        
        let ulAttrsD = ("class" =: "mdl-menu mdl-menu--bottom-left mdl-js-menu" <>)
                   <$> forDAttr
                       
        _ <- elDynAttr' "ul" ulAttrsD $
                listViewWithKey (Map.fromList . zip [0..] <$> valuesD) 
                              $ rowFunc defaultIndexD


        let selectionE = textResult ^. textInput_input
        return $ selectionE
            where
                defaultIndex Nothing _ = Nothing
                defaultIndex (Just x) xs = findIndex (x ==) xs
                defaultIndexD = defaultIndex
                            <$> mDefaultValueD
                            <*> valuesD
                forDAttr = ("for" =:) <$> htmlIdD

rowFunc :: forall t m v. (MonadWidget t m, Show v)
        => Dynamic t (Maybe Int)
        -> Int
        -> Dynamic t v
        -> m (Event t v)
rowFunc defaultValueD key valueD = do
    elDynAttr "li" dynAttrsD $ display valueD
    return never
    where
        classAttr = "class" =: "mdl-menu__item" 
        valueAttrD = ("data-val" =:)
                   . T.pack
                   . show <$>  valueD

        tmp = (classAttr <>) <$> valueAttrD
        dynAttrsD = mappend <$> tmp <*> defaultValueAttrD

        defaultValueAttr k (Just dk) = if k == dk
                                     then "data-selected" =: "true" 
                                     else Map.empty
        defaultValueAttr _ _   = Map.empty

        defaultValueAttrD = defaultValueAttr key
                        <$> defaultValueD

defaultSelectClasses :: [T.Text]
defaultSelectClasses = [ "mdl-textfield"
                       , "mdl-js-textfield"
                       , "mdl-textfield"
                       , "--floating-label"
                       , "getmdl-select"
                       , "getmdl-select__fix-height"
                       ]
