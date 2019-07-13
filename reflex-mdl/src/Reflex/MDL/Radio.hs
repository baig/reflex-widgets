{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
module Reflex.MDL.Radio where

import "base"       Data.Monoid ((<>))
import "text"       Data.Text (Text)
import "reflex-dom" Reflex.Dom
import              Reflex.MDL.Common (materialize')

mdlRadio :: MonadWidget t m
         => Dynamic t Text
         -- ^ HTML id
         -> Text
         -- ^ name
         -> Text
         -- ^ value
         -> Bool
         -- ^ Initial checked state
         -> Event t Bool
         -- ^ External value
         -> m (Event t Text)
mdlRadio htmlIdD
         radioName
         optionText
         initialChecked
         checkE = do
    checkedD <- holdDyn initialChecked checkE
    let attrsD = (\htmlId isChecked -> "type"  =: "radio"
                                    <> "name"  =: radioName
                                    <> "value" =: optionText
                                    <> "id"    =: htmlId
                                    <> "class" =: "mdl-radio__button"
                                    <> if isChecked
                                       then "checked" =: ""
                                       else mempty
                 ) <$> htmlIdD <*> checkedD

    (_, resultE) <- materialize' $ do
        let labelAttrsD = (\htmlId isChecked -> "class" =: ( "mdl-radio mdl-js-radio mdl-js-ripple-effect atidot-radio-group-member"
                                                          <> if isChecked
                                                                then " is-checked"
                                                                else ""
                                                           )
                                             <> "for"   =: htmlId
                          ) <$> htmlIdD <*> checkedD
        elDynAttr' "label" labelAttrsD $ do
            (el_, _) <- elDynAttr' "input" attrsD blank
            let xE = domEvent Click el_
            elClass "span" "mdl-radio__label" $ text optionText
            return $ optionText <$ xE

    return resultE
