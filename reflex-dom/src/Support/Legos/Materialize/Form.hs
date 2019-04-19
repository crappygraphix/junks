{-|
  Module      : Support.Legos.Materialize.Form
  Description : Supporting widgets for reusable form elements.
-}
module Support.Legos.Materialize.Form where

--------------------------------------------------------------------------------
import Data.Map ( Map )
import Reflex.Dom
--------------------------------------------------------------------------------
import CGX.Prelude hiding ( id, on, max, min )
--------------------------------------------------------------------------------
import qualified Support.Legos.Form as F
import Support.Legos.Types
--------------------------------------------------------------------------------

elDropdown :: (MonadWidget t m, Ord k)
  => Text
  -> Text
  -> TabIndex
  -> k
  -> Dynamic t (Map k Text)
  -> m (Dropdown t k)
elDropdown id label tabIndex initial options =
  el "label" $ do
    el "span" $ text label
    dropdown initial options $
      def & dropdownConfig_attributes .~ constDyn attrs
  where
    attrs = case tabIndex of
      TabIndex v -> "id" =: id <> "tabIndex" =: v <> "class" =: "browser-default"
      NoTabIndex -> "id" =: id <> "class" =: "browser-default"

elCheckbox :: MonadWidget t m
  => Text
  -> Text
  -> TabIndex
  -> Bool
  -> Event t Bool
  -> m (Checkbox t)
elCheckbox = F.elCheckboxR

elSwitch :: MonadWidget t m
  => Text
  -> Text
  -> Text
  -> TabIndex
  -> Bool
  -> Event t Bool
  -> m (Checkbox t)
elSwitch id off on tabIndex initial evSetValue =
  divClass "switch" $
    el "label" $ do
      el "span" $ text off
      cb <- checkbox initial $
        def & checkboxConfig_attributes .~ constDyn attrs
          & checkboxConfig_setValue .~ evSetValue
      elClass "span" "lever" blank
      el "span" $ text on
      return cb
  where
    attrs = case tabIndex of
      TabIndex v -> "id" =: id <> "tabIndex" =: v
      NoTabIndex -> "id" =: id

elRadio :: MonadWidget t m
  => Text
  -> Text
  -> Text
  -> TabIndex
  -> a
  -> Bool
  -> m (FormElement t, Dynamic t (Maybe a))
elRadio = F.elRadioR

elRange :: MonadWidget t m
  => Text
  -> Int
  -> Int
  -> TabIndex
  -> Int
  -> m (FormElement t, Dynamic t Int)
elRange id min max tabIndex initial =
  elClass "p" "range-field" $
    F.elRange id min max tabIndex initial

elTextInput :: MonadWidget t m
  => Text
  -> Text
  -> TabIndex
  -> Text
  -> Event t Text
  -> m (TextInput t)
elTextInput id label tabIndex dflt evSetValue =
  elClass "span" "input-field" $ do
    ele <- textInput $
      def & textInputConfig_inputType .~ "text"
        & textInputConfig_attributes .~ constDyn attrs
        & textInputConfig_setValue .~ evSetValue
        & textInputConfig_initialValue .~ dflt
    elAttr "label" ("for" =: id) $ el "span" $ text label
    return ele
  where
    attrs = case tabIndex of
      TabIndex v -> "id" =: id <> "tabIndex" =: v
      NoTabIndex -> "id" =: id

elPasswordInput :: MonadWidget t m
  => Text
  -> Text
  -> TabIndex
  -> Event t Text
  -> m (TextInput t)
elPasswordInput id label tabIndex evSetValue =
  elClass "span" "input-field" $ do
    ele <- textInput $
      def & textInputConfig_inputType .~ "password"
        & textInputConfig_attributes .~ constDyn attrs
        & textInputConfig_setValue .~ evSetValue
        & textInputConfig_initialValue .~ ""
    elAttr "label" ("for" =: id) $ el "span" $ text label
    return ele
  where
    attrs = case tabIndex of
      TabIndex v -> "id" =: id <> "tabIndex" =: v
      NoTabIndex -> "id" =: id
