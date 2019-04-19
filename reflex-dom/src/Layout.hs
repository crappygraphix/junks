{-# LANGUAGE RecursiveDo #-}

module Layout where

import Reflex.Dom

import CGX.Prelude
import Data.List (isInfixOf, isSuffixOf)
import GHCJS.DOM (currentWindowUnchecked)
import GHCJS.DOM.Location (getHref)
import GHCJS.DOM.Window (getLocation)
import Lens.Micro.Platform ((^.))

import qualified Auth as A
import qualified Nav as N
import qualified Group as G
import qualified Note as N
import Support.Legos.Router
import Support.Session
import Support.Routes

runLayout :: IO ()
runLayout = mainWidgetInElementById "main-body" body

body :: MonadWidget t m => m ()
body =
  elClass "div" "content" $ do
    rec
      -- Hack to support the fast switching frontend tech "r" is Reflex.
      loc <- getHref =<< getLocation =<< currentWindowUnchecked
      let root = if isInfixOf "/rd/" loc || isSuffixOf "/rd" loc
                 then "rd"
                 else ""

      dyEv <- holdDyn never evViews
      dyUa <- routePath root $ leftmost [switchPromptlyDyn dyEv, evNav]
      evNav <- N.nav dyUa

      -- What does "dyn" do? Given a Dynamic of widget-creating actions, create a widget that is recreated whenever
      -- the Dynamic updates. The returned Event of widget results occurs when the Dynamic does. Note: Often, the type
      -- 'a' is an Event, in which case the return value is an Event-of-Events that would typically be flattened
      -- (via 'switchPromptly').

      evViews <- dyn $ ffor dyUa $ \ua ->
        case ua ^. view of
          -- Not Auth
          VLogIn    -> A.login ua
          VRegister -> A.register ua
          VForgotPassword -> A.forgotPassword ua
          VResetPassword tkn -> A.resetPassword tkn ua

          -- Authed
          VNewGroup -> G.new ua
          VNewNote mGid -> N.new mGid ua
          VNote nid -> N.detail nid ua
          VEditNote nid -> N.edit nid ua
          VGroupSettings gid -> G.settings gid ua

          _ -> N.pageNotFound ua
    return ()
