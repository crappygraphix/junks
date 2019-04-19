{-|
  Module      : Support.Legos.Types
  Description : Shared generalized types.
-}

module Support.Legos.Types where

--------------------------------------------------------------------------------
import Reflex.Dom
--------------------------------------------------------------------------------
import CGX.Prelude
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

type FormElement t = InputElement EventResult GhcjsDomSpace t

data TabIndex = TabIndex Text | NoTabIndex
