{-|
  Module      : Support.Legos.Materialize.Button
  Description : Dependent on version 1.0.X of Materialize CSS available at https://materializecss.com

  The CSS and JS for MaterializeCSS must be available in the consuming app for these
  functions to render widgets correctly.

  Creates various buttons with stylings.
-}

module Support.Legos.Materialize.Button
  ( buttonStyleToClass
  , buttonStyleToClass'
  , elButton
  , elButtonDisableable
  , elCloseButton
  , elIconButton
  , elIconLink
  , elIconTextLink
  ) where

--------------------------------------------------------------------------------
import Data.Map ( Map )
import Reflex.Dom
--------------------------------------------------------------------------------
import CGX.Prelude
--------------------------------------------------------------------------------
import Support.Legos.Materialize.Icon
import Support.Legos.Materialize.Types
--------------------------------------------------------------------------------

-- | Translate a button's style to the CSS classes needed to render it
buttonStyleToClass :: ButtonStyle -> Map Text Text
buttonStyleToClass s = buttonStyleToClass' s ""

-- | Translate a button's style to the CSS classes, with the ability to add custom classes
buttonStyleToClass' :: ButtonStyle -> Text -> Map Text Text
buttonStyleToClass' s t =
  case s of
    BtnDefault o -> "class" =: ("waves-effect waves-light" <> opt o <> t)
    BtnPrimary o -> "class" =: ("waves-effect waves-light blue darken-2" <> opt o <> t)
    BtnInfo o    -> "class" =: ("waves-effect waves-light blue lighten-2" <> opt o <> t)
    BtnWarning o -> "class" =: ("waves-effect waves-light orange black-text" <> opt o <> t)
    BtnDanger o  -> "class" =: ("waves-effect waves-light red black-text" <> opt o <> t)
  where
    opt o =
      case o of
        None -> " btn "
        F    -> " btn-flat "
        FW   -> " btn fw "
        FFW  -> " btn-flat fw "

-- | A simple button widget
elButton :: DomBuilder t m
  => ButtonStyle
  -> Text
  -> m (Event t ())
elButton s t = do
  (b, _) <- elAttr' "a" (deadLink <> buttonStyleToClass s) $ text t
  return $ domEvent Click b

-- | A button widget that can be dynamically disabled
elButtonDisableable :: (DomBuilder t m, PostBuild t m)
  => ButtonStyle
  -> Text
  -> Dynamic t ButtonState
  -> m (Event t ())
elButtonDisableable s t dyB = do
  (b, _) <- elDynAttr' "a" (ffor dyB $ \b -> buttonStyleToClass' s (disabled b) <> deadLink) $ text t
  return $ domEvent Click b
  where
    disabled BtnDisabled = "disabled"
    disabled BtnEnabled = ""

-- | A button widget exclusively for closing a modal
elCloseButton :: DomBuilder t m => m (Event t ())
elCloseButton = do
  (b, _) <- divClass "right-align" $
    elAttr' "a" (deadLink <> buttonStyleToClass (BtnDefault F) <> "style" =: "padding:0") $ do
      elClass "i" "material-icons right" $ text "close"
      text "Close"
  return $ domEvent Click b

-- | A button widget with an icon denoted by a Materialize ligature, defaulted to the left side of the button
elIconButton :: DomBuilder t m
  => ButtonStyle
  -> IconId
  -> Text
  -> m (Event t ())
elIconButton s i t = do
  (b, _) <- elAttr' "a" (deadLink <> buttonStyleToClass s) (elIcon i IconLeft >> text t)
  return $ domEvent Click b

-- | A link widget with an icon denoted by a Materialize ligature
elIconLink :: DomBuilder t m
  => IconId
  -> m (Event t ())
elIconLink i = do
  (b, _) <- elAttr' "a" deadLink $ elIcon i IconNone
  return $ domEvent Click b

-- | A link widget with an icon denoted by a Materialize ligature followed by text
elIconTextLink :: DomBuilder t m
  => IconId
  -> Text
  -> m (Event t ())
elIconTextLink i t = do
  (b, _) <- elAttr' "a" deadLink $ do
    elIcon i IconNone
    elClass "span" "icon-link-text" $ text t
  return $ domEvent Click b

-- | Specifies a null address href
deadLink :: Map Text Text
deadLink = "href" =: "javascript:void(0);"
