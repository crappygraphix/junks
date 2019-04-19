module Support.Dom.FFI
  ( setFocus
  , setFocusEv
  ) where

-----
import Control.Monad.IO.Class
  ( MonadIO
  , liftIO
  )
import Data.Text
import Reflex.Dom
-----
import CGX.Prelude hiding ( id )
-----

-- | Focus the cursor on the given input field when the given event fires
setFocusEv :: MonadWidget t m
  => Text
  -> Event t a
  -> m (Event t ())
setFocusEv t = perform (const $ _setFocus ("#" <> t))

setFocus :: MonadWidget t m
  => Text
  -> m (Event t ())
setFocus t = perform (const $ _setFocus ("#" <> t)) =<< delay 0.1 =<< getPostBuild

-- | Execute the IO actions when the event occurs. The returned event contains results from those actions.
perform :: MonadWidget t m
  => (b -> IO a)
  -> Event t b
  -> m (Event t a)
perform f x = performEvent (fmap (liftIO . f) x)

-- | JQuery/Custom FFI for focusing on an HTML element
_setFocus :: MonadIO m => Text -> m ()
_setFocus t = liftIO $ jsAutoFocus t

foreign import javascript unsafe "autoFocus($1, 1000);"
  jsAutoFocus :: Text -> IO ()
