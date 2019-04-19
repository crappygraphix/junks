{-# LANGUAGE
    DeriveGeneric
  , TemplateHaskell
#-}

module Models.Errors where

import CGX.Prelude

import Data.Aeson
import Data.Aeson.Casing

import Models.Types

data ApiError
  = ApiError
  { apiErrorErrors :: [ErrorCode]
  } deriving (Show, Eq, Generic)

instance ToJSON ApiError where
  toJSON = genericToJSON $ aesonDrop 8 snakeCase
instance FromJSON ApiError where
  parseJSON = genericParseJSON $ aesonDrop 8 snakeCase
