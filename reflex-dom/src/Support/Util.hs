{-# LANGUAGE RecursiveDo #-}

module Support.Util where

-----
import Control.Monad.Fix
import Lens.Micro.Platform ( (^.) )
import Reflex
import Reflex.Dom
-----
import CGX.Prelude
import Models.Errors
import Models.Lenses
import Support.Legos.Flash
-----

flashConfig :: FlashConfig
flashConfig = FlashConfig "center-align" "black-text" "orange-text" "red-text"

lag :: MonadWidget t m => m (Event t ())
lag = delay 0.1 =<< getPostBuild

whenRight :: Reflex t => Event t (Either a b) -> Event t b
whenRight = fmapMaybe f
  where
    f x = case x of
      Left _ -> Nothing
      Right x' -> Just x'

whenLeft :: Reflex t => Event t (Either a b) -> Event t a
whenLeft = fmapMaybe f
  where
    f x = case x of
      Left x' -> Just x'
      Right _ -> Nothing

onceAll :: (Reflex t, MonadHold t m, MonadFix m) => [Event t a] -> m (Event t ())
onceAll l = do
  rec
    dyS <- holdDyn 0 $ attachWith (+) (current dyS) $ mergeWith (+) $ ffor l $ \e -> ffor e (const 1)
  return $ fforMaybe (updated dyS) $
    \c -> if c == length l
          then Just ()
          else Nothing

whenError :: Reflex t => Event t (Either ApiError a) -> Event t (Maybe Flash)
whenError ev = fmap unwrapEvent ev
  where
    unwrapEvent = \case
      Right _ -> Nothing
      Left ae -> (wrapIt . foldThem) (ae ^. errors)
    foldThem = foldr (\x (a, b) -> (a <> [userText x], b <> [consoleText x])) ([], [])
    wrapIt (a, b) = Just $ mkFlashErrC a b
