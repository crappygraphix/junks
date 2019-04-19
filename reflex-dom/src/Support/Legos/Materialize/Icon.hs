{-|
  Module      : Support.Legos.Materialize.Icon
  Description : Dependent on version 1.0.X of Materialize CSS available at https://materializecss.com

  The CSS and JS for MaterializeCSS must be available in the consuming app for these
  functions to render widgets correctly.
-}

module Support.Legos.Materialize.Icon where

--------------------------------------------------------------------------------
import Reflex.Dom
--------------------------------------------------------------------------------
import CGX.Prelude
--------------------------------------------------------------------------------
import Support.Legos.Materialize.Types
--------------------------------------------------------------------------------

elIcon :: DomBuilder t m => IconId -> IconOffset -> m ()
elIcon i o = elClass "i" c $ text i
  where
    c = case o of
      IconLeft -> "material-icons left"
      IconRight -> "material-icons right"
      IconNone -> "material-icons"

elDynIcon :: (DomBuilder t m, PostBuild t m) => Dynamic t IconId -> IconOffset -> m ()
elDynIcon i o = elClass "i" c $ dynText i
  where
    c = case o of
      IconLeft -> "material-icons left"
      IconRight -> "material-icons right"
      IconNone -> "material-icons"
