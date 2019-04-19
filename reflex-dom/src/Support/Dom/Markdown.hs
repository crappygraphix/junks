module Support.Dom.Markdown
( initialize
, getVal
, kill
, render
) where

-----
import Control.Monad.IO.Class
import Reflex.Dom hiding (Error)
-----
import CGX.Prelude hiding (id)
-----

initialize :: MonadWidget t m => Text -> Text -> Event t a -> m ()
initialize selector t ev = _perform (const (_initialize selector t)) (() <$ ev) >> return ()

getVal :: MonadWidget t m => Event t a -> m (Dynamic t Text)
getVal ev = do
  evF <- _perform (const _getVal) (() <$ ev)
  holdDyn "" evF

kill :: MonadWidget t m => Event t a -> m ()
kill ev = _perform (const _kill) (() <$ ev) >> return ()

render :: MonadWidget t m => Event t Text -> m (Dynamic t Text)
render ev = do
  evM <- _perform _render ev
  holdDyn "" evM

_initialize :: (MonadIO m) => Text -> Text -> m ()
_initialize a b = liftIO $ js_initialize a b

_getVal :: (MonadIO m) => m Text
_getVal = liftIO js_getVal

_kill :: (MonadIO m) => m ()
_kill = liftIO js_kill

_render :: (MonadIO m) => Text -> m Text
_render t = liftIO $ js_render t

_perform :: MonadWidget t m => (b -> IO a) -> Event t b -> m (Event t a)
_perform f x = performEvent (fmap (liftIO . f) x)

-- Javascript FFI Fuctions

-- Total Hack! Totally Dangerous! Puts an MDE instance in the GLOBAL SCOPE!!!
foreign import javascript safe "window.cgx = {mde: new EasyMDE({ element: $($1)[0], autoDownloadFontAwesome: true, hideIcons: ['guide'], autosave: true, initialValue: $2 })}"
  js_initialize :: Text -> Text -> IO ()

foreign import javascript safe "$r = window.cgx.mde.value();"
  js_getVal :: IO Text

foreign import javascript safe "window.cgx.mde = null;"
  js_kill :: IO ()

foreign import javascript safe "$r = marked($1, {breaks: true});"
  js_render :: Text -> IO Text
