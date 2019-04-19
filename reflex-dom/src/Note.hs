{-# LANGUAGE
    RecursiveDo
  , TypeFamilies
#-}

module Note (
  new
, detail
, edit
) where

-----
import Lens.Micro.Platform ( (^.) )
import qualified Lens.Micro.Platform as L ( view )
import Reflex.Dom
-----
import CGX.Prelude hiding ( id )
import Api.Routes
import Api.Xhr
import Auth
import Models.Content
import Models.Lenses
import Models.Types
import Support.Dom.Inputs
import qualified Support.Dom.Markdown as MK
import Support.Session
import qualified Support.Legos.Materialize as M
import Support.Legos.Flash
import Support.Util
import Support.Dom.FFI as F
import Support.Routes
-----

new :: MonadWidget t m
  => Maybe GroupId
  -> JunksState
  -> m (Event t JunksState)
new mGid ua = do
  ev <- lag
  case ua ^. session . authedUser of
    Nothing -> checkAuth ua ev
    Just _ -> main
  where
    main :: MonadWidget t m => m (Event t JunksState)
    main = do
      evTrigger <- lag
      evResp <- callApi Groups ua evTrigger
      let evDec = decodeShallowGroups evResp
          evW = fforMaybe (whenRight evDec) $ \case
            [] -> Nothing
            l@(h:_) -> find (\g -> g ^. id == fromMaybe (h ^. id) mGid) l

      dyEv <- widgetHold (blank >> return never) (ffor evW form)

      return $ switchPromptlyDyn dyEv

    form :: MonadWidget t m => GroupShallow -> m (Event t JunksState)
    form g = M.elTitleCardContainer "New Note" $ do
      evPb <- delay 0.1 =<< getPostBuild
      rec
        _ <- el "div" $ flashWidgetEv flashConfig (whenError evDec)
        el "h5" $ text (untag $ g ^. name)
        (_, _, dyTitle) <- noteTitle "new-note-title" (TabIndex "2") "" never
        _ <- noteBody "new-note-body" (TabIndex "3") "" never
        dyBody <- MK.getVal evClick
        _ <- MK.kill (whenRight evDec)
        _ <- MK.initialize "#new-note-body" "" evPb
        _ <- F.setFocus "new-note-title"
        evClick <- divClass "center-align row" $ M.elButton (M.BtnDefault M.None) "Create"
        evResp <- callApi (NewNote (NewNoteRequest <$> dyTitle <*> (mk <$> dyBody) <*> (constDyn (g ^. id)))) ua (updated dyBody)
        let evDec = decodeNoteResponse evResp
      return $ fmap (\r -> setView (VNote (r ^. id)) (Just TRefreshNav) ua) (whenRight evDec)

detail :: MonadWidget t m
  => NoteId
  -> JunksState
  -> m (Event t JunksState)
detail nid ua = do
  ev <- lag
  case ua ^. session . authedUser of
    Nothing -> checkAuth ua ev
    Just _ -> main
  where
    main :: MonadWidget t m => m (Event t JunksState)
    main = do
      evTrigger <- lag
      evResp <- callApi (GetNote (constDyn nid)) ua evTrigger
      let evDec = (whenRight . decodeNoteResponse) evResp

      dyEv <- widgetHold M.elLoadingWidgetEv (fmap w evDec)

      return $ switchPromptlyDyn dyEv

    w :: MonadWidget t m => NoteResponse -> m (Event t JunksState)
    w n = do
      ev <- getPostBuild
      dyBody <- MK.render (untag (n ^. body) <$ ev)
      ele <- divClass "container rendered-md" $ elDynHtml' "div" dyBody

      return $ fmap (\_ -> setView (VEditNote (n ^. id)) Nothing ua) (domEvent Dblclick ele)

edit :: MonadWidget t m
  => NoteId
  -> JunksState
  -> m (Event t JunksState)
edit nid ua = do
  ev <- lag
  case ua ^. session . authedUser of
    Nothing -> checkAuth ua ev
    Just _ -> main
  where
    main :: MonadWidget t m => m (Event t JunksState)
    main = do
      evTrigger <- lag
      evNoteResp <- callApi (GetNote (constDyn nid)) ua evTrigger
      let evNoteDec = (whenRight . decodeNoteResponse) evNoteResp
      evGroupResp <- callApi Groups ua $ () <$ evNoteDec
      let evGroupDec = (whenRight . decodeShallowGroups) evGroupResp
      rec
        dyTup <- holdDyn (Nothing, Nothing) $ attachWith tupify (current dyTup) (leftmost [Left <$> evNoteDec, Right <$> evGroupDec])
      let evTup = fmapMaybe tupped (updated dyTup)

      dyEv <- widgetHold (blank >> return never) (form <$> evTup)

      return $ switchPromptlyDyn dyEv
      where
        tupify (n, _) (Right g) = (n, Just g)
        tupify (_, g) (Left n) = (Just n, g)
        tupped (Just n, Just g) = Just (n, g)
        tupped _ = Nothing

    form :: MonadWidget t m => (NoteResponse, [GroupShallow]) -> m (Event t JunksState)
    form (n, gs) = M.elTitleCardContainer "Edit Note" $ do
      let mGroup = find (\g -> g ^. id == n ^. groupId) gs
      evPb <- delay 0.1 =<< getPostBuild
      rec
        _ <- el "div" $ flashWidgetEv flashConfig (whenError evDec)
        el "h5" $ text $ maybe "!!! Invalid Group ID !!!" (untag . L.view name) mGroup
        (_, _, dyTitle) <- noteTitle "edit-note-title" (TabIndex "2") (untag $ n ^. title) never
        _ <- noteBody "edit-note-body" (TabIndex "3") (untag $ n ^. body) never
        dyBody <- MK.getVal evClick
        _ <- MK.kill (whenRight evDec)
        _ <- MK.initialize "#edit-note-body" "" evPb
        _ <- F.setFocus "edit-note-title"
        evClick <- divClass "center-align row" $ M.elButton (M.BtnDefault M.None) "Save"
        evResp <- callApi (UpdateNote (constDyn (n ^. id)) (UpdateNoteRequest <$> dyTitle <*> (mk <$> dyBody))) ua (updated dyBody)
        let evDec = decodeNoteResponse evResp
      return $ fmap (\r -> setView (VNote (r ^. id)) (Just TRefreshNav) ua) (whenRight evDec)

-- TODO: Use something like this when changing a Note's Group
-- groupSelector :: MonadWidget t m => GroupId -> Map GroupId Text -> m (Dynamic t GroupId)
-- groupSelector dflt grps = do
--   (_, _, dyGid) <- selectGroup "new-note-group" (TabIndex "1") dflt (constDyn grps)
--   return dyGid
