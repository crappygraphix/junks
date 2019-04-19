{-|
  Module      : Support.Dom
  Description : CSS/JS Library independent functions
-}

module Support.Legos
  ( module Support.Legos
  , module Support.Legos.Button
  , module Support.Legos.Form
  , module Support.Legos.Types
  ) where

--------------------------------------------------------------------------------
import Data.Map ( Map )
import Reflex.Dom
--------------------------------------------------------------------------------
import CGX.Prelude
import Support.Legos.Button
import Support.Legos.Form
import Support.Legos.Types
--------------------------------------------------------------------------------

-- | HTML attribute to remove an element from the display
hide :: Map Text Text
hide = "style" =: "display:none"

-- | HTML attribute to display an element
show :: Map Text Text
show = "style" =: ""

-- | Masks a keypress code value
anyKey :: Word -> ()
anyKey _ = ()

-- | Builds an HTML attribute depending on class
classToAttr :: Maybe Text -> Text -> Map Text Text
classToAttr mt i =
  case mt of
    Just t -> "for" =: i <> "class" =: t
    Nothing -> "for" =: i

-- | Builds an HTML attribute depending on tab index
tabToAttr :: TabIndex -> Text -> Map Text Text
tabToAttr t i =
  case t of
    TabIndex n -> "id" =: i <> "tabIndex" =: n
    NoTabIndex -> "id" =: i
