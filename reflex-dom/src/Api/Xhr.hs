{-# LANGUAGE
    LambdaCase
#-}

module Api.Xhr where

import Lens.Micro.Platform ( (^.) )
import Data.Aeson
  ( FromJSON
  , eitherDecode
  )
import qualified Data.ByteString.Lazy as BL
import Data.Text ( pack )
import Data.Text.Encoding ( encodeUtf8 )
import Reflex.Dom

import CGX.Prelude
import Models.Auth
import Models.Content
import Models.Errors

mkFrontendError :: Text -> ApiError
mkFrontendError e = ApiError [mk e]

unknownError :: ApiError
unknownError = ApiError ["Unknown frontend error."]

badRequestError :: ApiError
badRequestError = ApiError ["Bad request error."]

decodeResponse :: (Reflex t, FromJSON j) => Event t (Maybe XhrResponse) -> Event t (Either ApiError j)
decodeResponse ev = ffor ev $ \case
  Just xhr -> checkStatus xhr
  Nothing -> Left badRequestError
  where
    checkStatus xhr
      | (isValid . _xhrResponse_status) xhr = decodeXhrResponse' xhr
      | otherwise = (Left . decodeError) xhr
    isValid word = word >= 200 && word < 300

decodeError :: XhrResponse -> ApiError
decodeError x = dec (_xhrResponse_responseText x)
  where
    dec (Just a) = case (eitherDecode . BL.fromStrict . encodeUtf8) a of
      Left s -> mkFrontendError ("JSON Error: " <> pack s <> " -- " <> a)
      Right o -> o
    dec Nothing = mkFrontendError "JSON Error: Empty Body"

-- | Convenience function to decode JSON and expose parsing errors.
decodeXhrResponse' :: FromJSON a => XhrResponse -> Either ApiError a
decodeXhrResponse' = dec . _xhrResponse_responseText
  where
    dec (Just a) = case (eitherDecode . BL.fromStrict . encodeUtf8) a of
      Left s -> Left $ mkFrontendError ("JSON Error: " <> pack s <> " -- " <> a)
      Right o -> Right o
    dec Nothing = Left $ mkFrontendError "JSON Error: Empty Body"

decodeEmpty :: Reflex t => Event t (Maybe XhrResponse) -> Event t (Either ApiError ())
decodeEmpty ev = ffor ev $ \case
  Just xhr -> toEither xhr
  Nothing -> Left badRequestError
  where
    toEither r = case r ^. xhrResponse_responseText of
      Nothing -> Right ()
      Just "" -> Right ()
      Just _e -> Left $ decodeError r

-- Payload Decoders

decodeAuthResponse :: Reflex t
  => Event t (Maybe XhrResponse)
  -> Event t (Either ApiError AuthResponse)
decodeAuthResponse = decodeResponse

decodeShallowGroups :: Reflex t
  => Event t (Maybe XhrResponse)
  -> Event t (Either ApiError [GroupShallow])
decodeShallowGroups = decodeResponse

decodeGroupResponse :: Reflex t
  => Event t (Maybe XhrResponse)
  -> Event t (Either ApiError GroupResponse)
decodeGroupResponse = decodeResponse

decodeNoteResponse :: Reflex t
  => Event t (Maybe XhrResponse)
  -> Event t (Either ApiError NoteResponse)
decodeNoteResponse = decodeResponse
