{-|
  Module      : Support.Legos.Materialize.Container
  Description : Supporting widgets for reusable content containers.
-}
module Support.Legos.Materialize.Container where

--------------------------------------------------------------------------------
import Reflex.Dom
--------------------------------------------------------------------------------
import CGX.Prelude
--------------------------------------------------------------------------------
-- import Support.Legos.Materialize.Types
--------------------------------------------------------------------------------

elCardContainer :: MonadWidget t m => m a -> m a
elCardContainer c = divClass "container" $ divClass "card-panel" c

elTitleCardContainer :: MonadWidget t m => Text -> m a -> m a
elTitleCardContainer t c = divClass "container" $ elTitleCard t c

elCard :: MonadWidget t m => m a -> m a
elCard = divClass "card-panel"

elTitleCard :: MonadWidget t m => Text -> m a -> m a
elTitleCard t c = do
  el "h5" $ text t
  elCard c
