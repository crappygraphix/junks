{-# LANGUAGE
    LambdaCase
#-}

{-|
  Module      : Support.Legos.Flash
  Description : All flash widgets that can be used across the frontend.
-}
module Support.Legos.Flash where

--------------------------------------------------------------------------------
import Control.Monad.IO.Class ( liftIO )
import Data.Text.IO ( putStrLn )
import Reflex
import Reflex.Dom
--------------------------------------------------------------------------------
import CGX.Prelude
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- | Flash Configuration
data FlashConfig
  = FlashConfig
  { fcClass :: Text
  , fcInfo :: Text
  , fcWarn :: Text
  , fcErr :: Text
  }
-- | ErrorMessage typeclass
class ErrorMessage e where
  userText :: e -> Text
  consoleText :: e -> Text

-- | Types of flash messages
data FlashType
  = FlashErr
  | FlashInfo
  | FlashWarn
  deriving (Show, Eq)

-- | A flash message is defined by its type and text. It can have two messages, one to display to the user and one
-- | to log in the console.
data Flash = Flash FlashType [Text] [Text] deriving (Show, Eq)

-- | Create a flash error message without a console component
mkFlashErr :: [Text] -> Flash
mkFlashErr ts = mkFlashErrC ts []

-- | Create a flash information message without a console component
mkFlashInfo :: [Text] ->Flash
mkFlashInfo ts = mkFlashInfoC ts []

-- | Create a flash warning message without a console component
mkFlashWarn :: [Text] ->Flash
mkFlashWarn ts = mkFlashWarnC ts []

-- | Create a flash error message with a console component
mkFlashErrC :: [Text] -> [Text] -> Flash
mkFlashErrC = Flash FlashErr

-- | Create a flash information message with a console component
mkFlashInfoC :: [Text] -> [Text] ->Flash
mkFlashInfoC = Flash FlashInfo

-- | Create a flash warning message with a console component
mkFlashWarnC :: [Text] -> [Text] ->Flash
mkFlashWarnC = Flash FlashWarn

-- | Translate a JSON error to a flash error message
errorMessageToFlash :: ErrorMessage e => e -> Flash
errorMessageToFlash e = mkFlashErrC [userText e] [consoleText e]

-- | Translate a JSON error to a flash error message, with the ability to add custom text to the user component
errorMessageToFlash' :: ErrorMessage e => e -> [Text] -> Flash
errorMessageToFlash' e ts = mkFlashErrC (ts <> [userText e]) [consoleText e]

-- | Translate an API response to a flash error message. If the response is valid (no error) then no
-- | flash error message is created.
responseError :: ErrorMessage e => Either e a -> Maybe Flash
responseError (Right _) = Nothing
responseError (Left e) = Just . errorMessageToFlash $ e

-- | Build a flash widget given a flash message. The console component of the message is put directly into the console
-- | while the user component is color formatted and displayed in an unordered list.
flashWidget :: MonadWidget t m => FlashConfig -> Maybe Flash -> m ()
flashWidget _ Nothing = blank
flashWidget c (Just f) = do
  _ <- liftIO . mapM putStrLn . console $ f
  _ <- elClass "ul" (fcClass c) $ simpleList (constDyn $ texts f) (flashRow $ flashType f)
  return ()
  where
    console (Flash _ _ ts) = ts
    texts (Flash _ ts _) = ts
    flashType (Flash t _ _) = t
    flashRow tp dyT = elClass "li" (color tp) (dynText dyT)
    color = \case
      FlashErr  -> (fcErr c)
      FlashWarn -> (fcWarn c)
      FlashInfo -> (fcInfo c)

-- | Build a flash widget given a flash message event. The console component of the message is put directly into the
-- | console while the user component is color formatted and displayed in an unordered list.
flashWidgetEv :: MonadWidget t m => FlashConfig -> Event t (Maybe Flash) -> m ()
flashWidgetEv c evF = do
  _ <- widgetHold blank (flashWidget c <$> evF)
  return ()

-- | Build a flash widget given a dynamic flash message. The console component of the message is put directly into the
-- | console while the user component is color formatted and displayed in an unordered list.
flashWidgetDyn :: MonadWidget t m => FlashConfig -> Dynamic t (Maybe Flash) -> m ()
flashWidgetDyn c dyF = do
  _ <- widgetHold blank (flashWidget c <$> (updated dyF))
  return ()

-- | Convert an error from an Either representation to a Maybe representation
eitherToMaybeError :: Either e a -> Maybe e
eitherToMaybeError (Left ae) = Just ae
eitherToMaybeError (Right _) = Nothing

-- | Display the given error on the standard output device
writeToConsole :: (MonadWidget t m, ErrorMessage e) => e -> m ()
writeToConsole e = do
  _ <- liftIO . putStrLn $ userText e
  _ <- liftIO . putStrLn $ consoleText e
  return ()
