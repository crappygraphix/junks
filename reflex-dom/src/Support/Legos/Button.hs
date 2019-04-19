{-|
  Module      : Support.Legos.Button
  Description : Button and link builders.
-}

module Support.Legos.Button
  ( elLink
  , elAttrLink
  , elTextLink
  , elTextAttrLink
  , elDynTextLink
  , elDynAttrLink
  , elDynTextAttrLink
  ) where

--------------------------------------------------------------------------------
import Data.Map ( Map )
import Reflex.Dom
--------------------------------------------------------------------------------
import CGX.Prelude
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | A button displayed as a hyperlink, wrapping the provided content
elLink :: DomBuilder t m
  => m ()
  -> m (Event t ())
elLink m = do
  (b, _) <- elAttr' "a" deadLink m
  return $ domEvent Click b

-- | A button displayed as a hyperlink with attributes, wrapping the provided content
elAttrLink :: DomBuilder t m
  => m ()
  -> Map Text Text
  -> m (Event t ())
elAttrLink m a = do
  (b, _) <- elAttr' "a" (a <> deadLink) m
  return $ domEvent Click b

-- | A button displayed as a hyperlink with text and default attributes

elTextLink :: DomBuilder t m
  => Text
  -> m (Event t ())
elTextLink t = elLink $ text t

-- | A button displayed as a hyperlink with text and attributes
elTextAttrLink :: DomBuilder t m
  => Text
  -> Map Text Text
  -> m (Event t ())
elTextAttrLink t a = elAttrLink (text t) a

-- | A button displayed as a hyperlink with dynamic text and default attributes
elDynTextLink :: (DomBuilder t m, PostBuild t m)
  => Dynamic t Text
  -> m (Event t ())
elDynTextLink dyT = elLink $ dynText dyT

-- | A button displayed as a hyperlink with text and dynamic attributes
elDynAttrLink :: (DomBuilder t m, PostBuild t m)
  => Text
  -> Dynamic t (Map Text Text)
  -> m (Event t ())
elDynAttrLink t dyA = do
  (b, _) <- elDynAttr' "a" (ffor dyA (\a -> a <> deadLink)) $ text t
  return $ domEvent Click b

-- | A button displayed as a hyperlink with dynamic text and dynamic attributes
elDynTextAttrLink :: (DomBuilder t m, PostBuild t m)
  => Dynamic t Text
  -> Dynamic t (Map Text Text)
  -> m (Event t ())
elDynTextAttrLink dyT dyA = do
  (b, _) <- elDynAttr' "a" (ffor dyA (\a -> a <> deadLink)) $ dynText dyT
  return $ domEvent Click b

-- | Specifies a null address href
deadLink :: Map Text Text
deadLink = "href" =: "javascript:void(0);"
