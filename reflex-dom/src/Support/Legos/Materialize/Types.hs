{-|
  Module      : Support.Legos.Materialize
  Description : Dependent on version 1.0.X of Materialize CSS available at https://materializecss.com

  The CSS and JS for MaterializeCSS must be available in the consuming app for these
  functions to render widgets correctly.
-}

module Support.Legos.Materialize.Types where

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
import CGX.Prelude
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Types of buttons
data ButtonOption
  = None
  | F   -- ^ Flat
  | FW  -- ^ Full Width
  | FFW -- ^ Flat Full Width

-- | Button colors
data ButtonStyle
  = BtnDefault ButtonOption
  | BtnPrimary ButtonOption
  | BtnInfo ButtonOption
  | BtnWarning ButtonOption
  | BtnDanger ButtonOption

-- | Button states
data ButtonState
  = BtnDisabled
  | BtnEnabled

-- | An icon identifier for cleaner function signatures
type IconId = Text

-- | Positions to anchor an icon within a button
data IconOffset
  = IconLeft
  | IconRight
  | IconNone
