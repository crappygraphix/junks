{-# LANGUAGE
    DeriveGeneric
  , TemplateHaskell
#-}

module Models.Auth where

import CGX.Prelude
import Data.Aeson
import Data.Aeson.Casing
import qualified Data.Text as T

import Models.Types

data AuthRequest
  = AuthRequest
  { authRequestEmail    :: EmailAddress
  , authRequestPassword :: Password
  } deriving (Show, Eq, Generic)

instance ToJSON AuthRequest where
  toJSON = genericToJSON $ aesonDrop (T.length "AuthRequest") snakeCase
instance FromJSON AuthRequest where
  parseJSON = genericParseJSON $ aesonDrop (T.length "AuthRequest") snakeCase

data User
  = User
  { userId :: UserId
  , userName :: Username
  , userEmail :: EmailAddress
  } deriving (Show, Eq, Generic)

instance ToJSON User where
  toJSON = genericToJSON $ aesonDrop (T.length "User") snakeCase
instance FromJSON User where
  parseJSON = genericParseJSON $ aesonDrop (T.length "User") snakeCase

data AuthResponse
  = AuthResponse
  { authResponseToken :: AuthToken
  , authResponseUser  :: User
  } deriving (Show, Eq, Generic)

instance ToJSON AuthResponse where
  toJSON = genericToJSON $ aesonDrop (T.length "AuthResponse") snakeCase
instance FromJSON AuthResponse where
  parseJSON = genericParseJSON $ aesonDrop (T.length "AuthResponse") snakeCase

data RegisterRequest
  = RegisterRequest
  { registerRequestEmail     :: EmailAddress
  , registerRequestName      :: Username
  , registerRequestPassword1 :: Password
  , registerRequestPassword2 :: Password
  } deriving (Show, Eq, Generic)

instance ToJSON RegisterRequest where
  toJSON = genericToJSON $ aesonDrop (T.length "RegisterRequest") snakeCase
instance FromJSON RegisterRequest where
  parseJSON = genericParseJSON $ aesonDrop (T.length "RegisterRequest") snakeCase

data ForgotPWRequest
  = ForgotPWRequest
  { forgotPWRequestEmail     :: EmailAddress
  } deriving (Show, Eq, Generic)

instance ToJSON ForgotPWRequest where
  toJSON = genericToJSON $ aesonDrop (T.length "ForgotPWRequest") snakeCase
instance FromJSON ForgotPWRequest where
  parseJSON = genericParseJSON $ aesonDrop (T.length "ForgotPWRequest") snakeCase

data ResetPWRequest
  = ResetPWRequest
  { resetPWRequestResetToken :: ResetToken
  , resetPWRequestPassword1  :: Password
  , resetPWRequestPassword2  :: Password
  } deriving (Show, Eq, Generic)

instance ToJSON ResetPWRequest where
  toJSON = genericToJSON $ aesonDrop (T.length "ResetPWRequest") snakeCase
instance FromJSON ResetPWRequest where
  parseJSON = genericParseJSON $ aesonDrop (T.length "ResetPWRequest") snakeCase
