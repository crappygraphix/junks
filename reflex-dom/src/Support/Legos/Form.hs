{-|
  Module      : Support.Legos.Form
  Description : Form element builders.
-}

module Support.Legos.Form
( elDropdown
, elCheckboxL
, elCheckboxR
, elRadioL
, elRadioR
, elRange
, elPasswordInput
, elTextInput
, elTextArea
) where

--------------------------------------------------------------------------------
import Data.Map ( Map )
import qualified Data.Text as T
import Reflex.Dom hiding ( value )
import Text.Read ( readMaybe )
--------------------------------------------------------------------------------
import CGX.Prelude hiding ( id, max, min )
--------------------------------------------------------------------------------
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
      TabIndex v -> "id" =: id <> "tabIndex" =: v
      NoTabIndex -> "id" =: id

elCheckboxL :: MonadWidget t m
  => Text
  -> Text
  -> TabIndex
  -> Bool
  -> Event t Bool
  -> m (Checkbox t)
elCheckboxL id label tabIndex initial evSetValue =
  el "label" $ do
    el "span" $ text label
    checkbox initial $
      def & checkboxConfig_attributes .~ constDyn attrs
        & checkboxConfig_setValue .~ evSetValue
  where
    attrs = case tabIndex of
      TabIndex v -> "id" =: id <> "tabIndex" =: v
      NoTabIndex -> "id" =: id

elCheckboxR :: MonadWidget t m
  => Text
  -> Text
  -> TabIndex
  -> Bool
  -> Event t Bool
  -> m (Checkbox t)
elCheckboxR id label tabIndex initial evSetValue =
  el "label" $ do
    cb <- checkbox initial $
      def & checkboxConfig_attributes .~ constDyn attrs
        & checkboxConfig_setValue .~ evSetValue
    el "span" $ text label
    return cb
  where
    attrs = case tabIndex of
      TabIndex v -> "id" =: id <> "tabIndex" =: v
      NoTabIndex -> "id" =: id

elRadioL :: MonadWidget t m
  => Text
  -> Text
  -> Text
  -> TabIndex
  -> a
  -> Bool
  -> m (FormElement t, Dynamic t (Maybe a))
elRadioL id label group tabindex value selected = do
  (elR, ev) <- el "label" $ do
    el "span" $ text label
    (elR, evVal) <- elRadio id group tabindex selected
    return (elR, fmap (justTrue value) evVal)
  dyV <- holdDyn (justTrue value selected) ev
  return (elR, dyV)

elRadioR :: MonadWidget t m
  => Text
  -> Text
  -> Text
  -> TabIndex
  -> a
  -> Bool
  -> m (FormElement t, Dynamic t (Maybe a))
elRadioR id label group tabIndex value selected = do
  (elR, ev) <- el "label" $ do
    (elR, evVal) <- elRadio id group tabIndex selected
    el "span" $ text label
    return (elR, fmap (justTrue value) evVal)
  dyV <- holdDyn (justTrue value selected) ev
  return (elR, dyV)

justTrue :: a -> Bool -> Maybe a
justTrue a b =
  if b
  then Just a
  else Nothing

elRadio :: MonadWidget t m
  => Text
  -> Text
  -> TabIndex
  -> Bool
  -> m (FormElement t, Event t Bool)
elRadio id group tabIndex selected = do
  ie <- inputElement $ def
    & inputElementConfig_initialChecked .~ selected
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ attrs
  return (ie, _inputElement_checkedChange ie)
  where
    attrs = case tabIndex of
      TabIndex v -> "id" =: id <> "name" =: group <> "type" =: "radio" <> "tabIndex" =: v
      NoTabIndex -> "id" =: id <> "name" =: group <> "type" =: "radio"

elRange :: MonadWidget t m
  => Text
  -> Int
  -> Int
  -> TabIndex
  -> Int
  -> m (FormElement t, Dynamic t Int)
elRange id min max tabIndex initial = do
  ie <- inputElement $ def
    & inputElementConfig_initialValue .~ (ps initial)
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ attrs
  return $ (ie, sp <$> _inputElement_value ie)
  where
    sp s = fromMaybe 0 (readMaybe . T.unpack $ s)
    attrs = case tabIndex of
      TabIndex v -> "id" =: id <> "type" =: "range" <> "min" =: (ps min) <> "max" =: (ps max) <> "tabIndex" =: v
      NoTabIndex -> "id" =: id <> "type" =: "range" <> "min" =: (ps min) <> "max" =: (ps max)

elPasswordInput :: MonadWidget t m
  => Text
  -> Text
  -> TabIndex
  -> Event t Text
  -> m (TextInput t)
elPasswordInput id label tabIndex evSetValue =
  el "label" $ do
    el "span" $ text label
    textInput $
      def & textInputConfig_inputType .~ "password"
        & textInputConfig_attributes .~ constDyn attrs
        & textInputConfig_setValue .~ evSetValue
        & textInputConfig_initialValue .~ ""
  where
    attrs = case tabIndex of
      TabIndex v -> "id" =: id <> "tabIndex" =: v
      NoTabIndex -> "id" =: id

elTextInput :: MonadWidget t m
  => Text
  -> Text
  -> TabIndex
  -> Text
  -> Event t Text
  -> m (TextInput t)
elTextInput id label tabIndex dflt evSetValue =
  el "label" $ do
    el "span" $ text label
    textInput $
      def & textInputConfig_inputType .~ "text"
        & textInputConfig_attributes .~ constDyn attrs
        & textInputConfig_setValue .~ evSetValue
        & textInputConfig_initialValue .~ dflt
  where
    attrs = case tabIndex of
      TabIndex v -> "id" =: id <> "tabIndex" =: v
      NoTabIndex -> "id" =: id

elTextArea :: MonadWidget t m
  => Text
  -> TabIndex
  -> Text
  -> Event t Text
  -> m (TextArea t)
elTextArea id tabIndex dflt ev =
  textArea $ def
    & textAreaConfig_attributes .~ constDyn attrs
    & textAreaConfig_initialValue .~ dflt
    & textAreaConfig_setValue .~ ev
  where
    attrs = case tabIndex of
      TabIndex v -> "id" =: id <> "tabIndex" =: v
      NoTabIndex -> "id" =: id
