{-# LANGUAGE
    RecursiveDo
  , LambdaCase
  , TypeFamilies
#-}

module Group (
  new
, settings
) where

-----
import Lens.Micro.Platform ( (^.) )
import Reflex.Dom
-----
import CGX.Prelude hiding ( id )
import Api.Routes
import Api.Xhr
import Auth
import Models.Content
import Models.Lenses
import Models.Types
import Support.Session
import qualified Support.Dom.Inputs as I
import Support.Legos
import qualified Support.Legos.Materialize as M
import Support.Legos.Flash
import Support.Util
import Support.Dom.FFI as F
import Support.Routes
-----

new :: MonadWidget t m
  => JunksState
  -> m (Event t JunksState)
new ua = do
  ev <- lag
  case ua ^. session . authedUser of
    Nothing -> checkAuth ua ev
    Just _ -> main
  where
    main :: MonadWidget t m => m (Event t JunksState)
    main = form

    form :: MonadWidget t m => m (Event t JunksState)
    form = M.elTitleCardContainer "New Folder" $ do
      rec
        _ <- el "div" $ flashWidgetEv flashConfig (whenError evDec)
        (_, _, dyName) <- I.groupName "new-group-name" (TabIndex "1") never
        evClick <- divClass "center-align row" $ M.elButton (M.BtnDefault M.None) "Create"
        _ <- F.setFocus "new-group-name"

        evResp <- callApi (NewGroup (NewGroupRequest <$> dyName)) ua evClick
        let evDec = decodeGroupResponse evResp
      return $ fmap (\r -> setView (VGroupSettings (r ^. id)) (Just TRefreshNav) ua) (whenRight evDec)

settings :: MonadWidget t m
  => GroupId
  -> JunksState
  -> m (Event t JunksState)
settings _gid ua = do
  ev <- lag
  case ua ^. session . authedUser of
    Nothing -> checkAuth ua ev
    Just _ -> main
  where
    main :: MonadWidget t m => m (Event t JunksState)
    main = M.elTitleCardContainer "Folder Settings" $ do
      el "p" $ text "Settings will go here... yup."
      return never

    -- form :: MonadWidget t m => m (Event t JunksState)
    -- form = elWrapper "New Group" $ do
    --   rec
    --     _ <- el "div" $ flashWidgetEv (whenLeft evDec)
    --     (_, _, dyName) <- groupName "new-group-name" (TabIndex "1") never
    --     evClick <- divClass "center-align" $ elBtnP "Create"
    --     _ <- F.setFocus "#new-group-name"

    --     evResp <- callApi (NewGroup (NewGroupRequest <$> dyName)) ua evClick
    --     let evDec = decodeNewGroupResponse evResp
    --   return $ fmap (\r -> setView (VGroupSettings (r ^. id)) (Just TRefreshNav) ua) (whenRight evDec)
