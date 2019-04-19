{-# LANGUAGE
    DeriveGeneric
#-}

module Models.Content where

-----
import Data.Aeson
import Data.Aeson.Casing
import qualified Data.Text as T
-----
import CGX.Prelude
import Models.Types
-----

data NoteShallow
  = NoteShallow
  { noteShallowId :: NoteId
  , noteShallowTitle :: NoteTitle
  } deriving (Show, Eq, Generic)

instance ToJSON NoteShallow where
  toJSON = genericToJSON $ aesonDrop (T.length "NoteShallow") snakeCase
instance FromJSON NoteShallow where
  parseJSON = genericParseJSON $ aesonDrop (T.length "NoteShallow") snakeCase

data GroupShallow
  = GroupShallow
  { groupShallowId :: GroupId
  , groupShallowName :: GroupName
  , groupShallowNotes :: [NoteShallow]
  } deriving (Show, Eq, Generic)

instance ToJSON GroupShallow where
  toJSON = genericToJSON $ aesonDrop (T.length "GroupShallow") snakeCase
instance FromJSON GroupShallow where
  parseJSON = genericParseJSON $ aesonDrop (T.length "GroupShallow") snakeCase

data NewGroupRequest
  = NewGroupRequest
  { newGroupRequestName :: GroupName
  } deriving (Show, Eq, Generic)

instance ToJSON NewGroupRequest where
  toJSON = genericToJSON $ aesonDrop (T.length "NewGroupRequest") snakeCase
instance FromJSON NewGroupRequest where
  parseJSON = genericParseJSON $ aesonDrop (T.length "NewGroupRequest") snakeCase

data GroupResponse
  = GroupResponse
  { groupResponseId :: GroupId
  , groupResponseName :: GroupName
  } deriving (Show, Eq, Generic)

instance ToJSON GroupResponse where
  toJSON = genericToJSON $ aesonDrop (T.length "GroupResponse") snakeCase
instance FromJSON GroupResponse where
  parseJSON = genericParseJSON $ aesonDrop (T.length "GroupResponse") snakeCase

data NewNoteRequest
  = NewNoteRequest
  { newNoteRequestTitle :: NoteTitle
  , newNoteRequestBody :: NoteBody
  , newNoteRequestGroupId :: GroupId
  } deriving (Show, Eq, Generic)

instance ToJSON NewNoteRequest where
  toJSON = genericToJSON $ aesonDrop (T.length "NewNoteRequest") snakeCase
instance FromJSON NewNoteRequest where
  parseJSON = genericParseJSON $ aesonDrop (T.length "NewNoteRequest") snakeCase

data UpdateNoteRequest
  = UpdateNoteRequest
  { updateNoteRequestTitle :: NoteTitle
  , updateNoteRequestBody :: NoteBody
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateNoteRequest where
  toJSON = genericToJSON $ aesonDrop (T.length "UpdateNoteRequest") snakeCase
instance FromJSON UpdateNoteRequest where
  parseJSON = genericParseJSON $ aesonDrop (T.length "UpdateNoteRequest") snakeCase

data NoteResponse
  = NoteResponse
  { noteResponseGroupId :: GroupId
  , noteResponseId :: NoteId
  , noteResponseTitle :: NoteTitle
  , noteResponseBody :: NoteBody
  } deriving (Show, Eq, Generic)

instance ToJSON NoteResponse where
  toJSON = genericToJSON $ aesonDrop (T.length "NoteResponse") snakeCase
instance FromJSON NoteResponse where
  parseJSON = genericParseJSON $ aesonDrop (T.length "NoteResponse") snakeCase

data NewTagRequest
  = NewTagRequest
  { newTagRequestName :: TagName
  } deriving (Show, Eq, Generic)

instance ToJSON NewTagRequest where
  toJSON = genericToJSON $ aesonDrop (T.length "NewTagRequest") snakeCase
instance FromJSON NewTagRequest where
  parseJSON = genericParseJSON $ aesonDrop (T.length "NewTagRequest") snakeCase

data TagResponse
  = TagResponse
  { tagResponseId :: TagId
  , tagResponseName :: TagName
  } deriving (Show, Eq, Generic)

instance ToJSON TagResponse where
  toJSON = genericToJSON $ aesonDrop (T.length "TagResponse") snakeCase
instance FromJSON TagResponse where
  parseJSON = genericParseJSON $ aesonDrop (T.length "TagResponse") snakeCase
