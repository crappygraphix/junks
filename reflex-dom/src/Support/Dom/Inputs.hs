module Support.Dom.Inputs
  ( TabIndex (..)
  , email
  , password
  , passwordC
  , username
  , selectGroup
  , noteTitle
  , noteBody
  , groupName
  ) where

-----
import Data.Map (Map)
import Lens.Micro.Platform ((^.))
import Reflex.Dom
-----
import CGX.Prelude hiding (id)
import Models.Types
import Support.Legos
import qualified Support.Legos.Materialize as M
-----

email :: MonadWidget t m
  => Text
  -> TabIndex
  -> Event t Text
  -> m (TextInput t, Event t (), Dynamic t EmailAddress)
email id tabIndex evSetValue = do
  t <- elTextInput id "Email" tabIndex "" evSetValue
  return (t, keypress Enter t, fmap mk (t ^. textInput_value))

username :: MonadWidget t m
  => Text
  -> TabIndex
  -> Event t Text
  -> m (TextInput t, Event t (), Dynamic t Username)
username id tabIndex evSetValue = do
  t <- elTextInput id "Display Name" tabIndex "" evSetValue
  return (t, keypress Enter t, fmap mk (t ^. textInput_value))

password :: MonadWidget t m
  => Text
  -> TabIndex
  -> Event t Text
  -> m (TextInput t, Event t (), Dynamic t Password)
password id tabIndex evSetValue = do
  t <- elPasswordInput id "Password" tabIndex evSetValue
  return (t, keypress Enter t, fmap mk (t ^. textInput_value))

passwordC :: MonadWidget t m
  => Text
  -> TabIndex
  -> Event t Text
  -> m (TextInput t, Event t (), Dynamic t Password)
passwordC id tabIndex evSetValue = do
  t <- elPasswordInput id "Confirm Password" tabIndex evSetValue
  return (t, keypress Enter t, fmap mk (t ^. textInput_value))

groupName :: MonadWidget t m
  => Text
  -> TabIndex
  -> Event t Text
  -> m (TextInput t, Event t (), Dynamic t GroupName)
groupName id tabIndex evSetValue = do
  t <- elTextInput id "Name" tabIndex "" evSetValue
  return (t, keypress Enter t, fmap mk (t ^. textInput_value))

noteTitle :: MonadWidget t m
  => Text
  -> TabIndex
  -> Text
  -> Event t Text
  -> m (TextInput t, Event t (), Dynamic t NoteTitle)
noteTitle id tabIndex dflt evSetValue = do
  t <- elTextInput id "Title" tabIndex dflt evSetValue
  return (t, keypress Enter t, fmap mk (t ^. textInput_value))

noteBody :: MonadWidget t m
  => Text
  -> TabIndex
  -> Text
  -> Event t Text
  -> m (TextArea t, Event t Word, Dynamic t NoteBody)
noteBody id tabIndex dflt evSetValue = do
  t <- elTextArea id tabIndex dflt evSetValue
  return (t, t ^. textArea_keypress, fmap mk (t ^. textArea_value))

selectGroup :: MonadWidget t m
  => Text
  -> TabIndex
  -> GroupId
  -> Dynamic t (Map GroupId Text)
  -> m (Dropdown t GroupId, Event t GroupId, Dynamic t GroupId)
selectGroup id tabIndex initial options = do
  g <- M.elDropdown id "Group" tabIndex initial options
  return (g, g ^. dropdown_change, g ^. dropdown_value)
