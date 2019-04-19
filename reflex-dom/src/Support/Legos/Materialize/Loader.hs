{-|
  Module      : Support.Legos.Materialize.Loader
  Description : Loading widgets that can be used across the frontend.
-}
module Support.Legos.Materialize.Loader where

--------------------------------------------------------------------------------
import Reflex
import Reflex.Dom
--------------------------------------------------------------------------------
import CGX.Prelude
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Linear loader with default text
elLoadingWidget :: MonadWidget t m => m ()
elLoadingWidget = divClass "container" $ do
  elClass "h4" "center-align" $ text "Loading"
  divClass "progress" $
    divClass "indeterminate" blank

-- | Circular spinner with no text
elLoadingWidgetO :: MonadWidget t m => m ()
elLoadingWidgetO = divClass "container center-align" $
  divClass "preloader-wrapper active" $
    divClass "spinner-layer" $ do
      divClass "circle-clipper left" $ divClass "circle" blank
      divClass "gap-patch" $ divClass "circle" blank
      divClass "circle-clipper right" $ divClass "circle" blank

-- | Linear loader with default text
elLoadingWidgetEv :: MonadWidget t m => m (Event t a)
elLoadingWidgetEv = divClass "container" $ do
  elClass "h4" "center-align" $ text "Loading"
  divClass "progress" $
    divClass "indeterminate" blank
  return never

-- | Circular spinner with no text
elLoadingWidgetEvO :: MonadWidget t m => m (Event t a)
elLoadingWidgetEvO = divClass "container center-align" $
  divClass "preloader-wrapper active" $
    divClass "spinner-layer" $ do
      divClass "circle-clipper left" $ divClass "circle" blank
      divClass "gap-patch" $ divClass "circle" blank
      divClass "circle-clipper right" $ divClass "circle" blank
      return never
