{-# LANGUAGE FlexibleInstances #-}

module Models.Types where

import Data.Text.Read (decimal)

import CGX.Prelude
import Web.HttpApiData
import Support.Legos.Flash

urlId :: Text -> Either Text (Tagged a Int)
urlId p =
  case decimal p of
    Right (v, _) -> Right . mk . fromInteger $ v
    Left e -> Left . pack $ e

data NoteIdTag
type NoteId = Tagged NoteIdTag Int
instance FromHttpApiData NoteId where
  parseUrlPiece = urlId
instance ToHttpApiData NoteId where
  toUrlPiece = psu

data NoteTitleTag
type NoteTitle = Tagged NoteTitleTag Text

data NoteBodyTag
type NoteBody = Tagged NoteBodyTag Text

data TagIdTag
type TagId = Tagged TagIdTag Int
instance FromHttpApiData TagId where
  parseUrlPiece = urlId
instance ToHttpApiData TagId where
  toUrlPiece = psu

data TagNameTag
type TagName = Tagged TagNameTag Text

data GroupIdTag
type GroupId = Tagged GroupIdTag Int
instance FromHttpApiData GroupId where
  parseUrlPiece = urlId
instance ToHttpApiData GroupId where
  toUrlPiece = psu

data GroupNameTag
type GroupName = Tagged GroupNameTag Text

data ErrorCodeTag
type ErrorCode = Tagged ErrorCodeTag Text

instance ErrorMessage ErrorCode where
  userText e = case untag e of
    "BAD_AUTH" -> "Bad email or password."
    "PASSWORD_MISMATCH" -> "Passwords don't match"
    "PASSWORD_IN_MISSING" -> "Password is missing."
    "NAME_MISSING" -> "Name is missing."
    "EMAIL_MISSING" -> "Email is missing."
    "EMAIL_EXISTS" -> "Email is already registered."
    "BAD_TOKEN" -> "Token is no longer valid."
    "GROUP_NAME_USER_EXISTS" -> "Name already in use."
    "TITLE_MISSING" -> "Title is required."
    x -> "A system error has occurred. Please try refreshing the page. [" <> x <> "]"
  consoleText e = untag e

data UsernameTag
type Username = Tagged UsernameTag Text

data UserIdTag
type UserId = Tagged UserIdTag Int
instance FromHttpApiData UserId where
  parseUrlPiece = urlId
instance ToHttpApiData UserId where
  toUrlPiece = psu

data EmailAddressTag
type EmailAddress = Tagged EmailAddressTag Text

data PasswordTag
type Password = Tagged PasswordTag Text

data AuthTokenTag
type AuthToken = Tagged AuthTokenTag Text

data ResetTokenTag
type ResetToken = Tagged ResetTokenTag Text
