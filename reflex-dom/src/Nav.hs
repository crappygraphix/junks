module Nav
  ( nav
  , pageNotFound
  ) where

-----
import Lens.Micro.Platform ( (^.) )
import Reflex.Dom
-----
import CGX.Prelude hiding ( id )
import Auth
import Api.Routes
import Api.Xhr
import Models.Content
import Models.Lenses
import Support.Legos
import Support.Legos.Flash
import qualified Support.Legos.Materialize as M
import Support.Routes
import Support.Session
import Support.Util
-----

pageNotFound ::
     MonadWidget t m
  => JunksState
  -> m (Event t JunksState)
pageNotFound _ua = do
  M.elCardContainer $
    elClass "p" "center-align" $ text "We got 404 pages, but this ain't one."
  return never

data MenuVisibility = ShowMenu | HideMenu

nav :: MonadWidget t m => Dynamic t JunksState -> m (Event t JunksState)
nav dyUa = do
  dyEv <- widgetHold (menu (defaultJS, HideMenu)) (menu <$> authChanged (updated dyUa))
  return $ switchPromptlyDyn dyEv
  where
    authChanged :: Reflex t => Event t JunksState -> Event t (JunksState, MenuVisibility)
    authChanged evUa = fforMaybe evUa $
      \ua -> case ua ^. trigger of
        Just TLoggedIn -> Just (ua, ShowMenu)
        Just TRefreshNav -> Just (ua, ShowMenu)
        Just TLoggedOut -> Just (ua, HideMenu)
        _ -> Nothing

    menu :: MonadWidget t m => (JunksState, MenuVisibility) -> m (Event t JunksState)
    menu (_, HideMenu) = do
      _<- el "nav" $
        divClass "nav-wrapper" $
          elClass "span" "brand-logo center" $ text "Junks"
      return never

    menu (ua, ShowMenu) = do
      evTrigger <- lag
      evResp <- callApi Groups ua evTrigger
      let evDec = decodeShallowGroups evResp
      _ <- el "div" $ flashWidgetEv flashConfig (whenError evDec)

      _ <- el "nav" $ divClass "nav-wrapper" $ do
        _<- elClass "ul" "left" $ el "li" $ elAttrLink (M.elIcon "view_headline" M.IconNone) ("class" =: "show-on-large sidenav-trigger" <> "data-target" =: "left-nav")
        elAttrLink (text "Junks") ("class" =: "brand-logo center")

      elAttr "ul" ("id" =: "left-nav" <> "class" =: "sidenav edge-left") $ do
        dyEv <- widgetHold (return never) (groups ua <$> whenRight evDec)
        return $ switchPromptlyDyn dyEv

    groups :: MonadWidget t m => JunksState -> [GroupShallow] -> m (Event t JunksState)
    groups ua gl = do
      evT <- mapM (groupLi ua) gl
      evN <- elClass "li" "sidenav-close" $ do
        evClick <- M.elIconTextLink "create_new_folder" "Create Folder"
        return (setView VNewGroup Nothing ua <$ evClick)
      evX <- userActions
      return $ leftmost [evX, evN, leftmost evT]

    userActions :: MonadWidget t m => m (Event t JunksState)
    userActions = do
      evLo <- destroyAccessToken =<< elClass "li" "sidenav-close" (M.elIconTextLink "undo" "Log Out")
      return $ logOutJS <$ evLo

    groupLi :: MonadWidget t m => JunksState -> GroupShallow -> m (Event t JunksState)
    groupLi ua g = el "li" $ elClass "ul" "collapsible collapsible-accordion left-nav-dd" $
      el "li" $ do
        _ <- elAttrLink gn ("class" =: "collapsible-header waves-effect")
        divClass "collapsible-body" $ el "ul" $ do
          evDl <- mapM (noteLi ua) (g ^. notes)
          ev <- elClass "li" "sidenav-close" (M.elIconTextLink "note_add" "Create Note")
          el "li" $ divClass "divider" blank
          return (leftmost [setView (VNewNote (Just (g ^. id))) Nothing ua <$ ev, leftmost evDl])
      where
        gn = do
          M.elIcon "folder_open" M.IconNone
          elClass "span" "sidenav-folder-text" $ text $ untag (g ^. name)
          M.elIcon "arrow_drop_down" M.IconRight

    noteLi :: MonadWidget t m => JunksState -> NoteShallow -> m (Event t JunksState)
    noteLi ua n = do
      ev <- elClass "li" "sidenav-close" $ M.elIconTextLink "insert_drive_file" (untag (n ^. title)) -- nn
      return (setView (VNote (n ^. id)) Nothing ua <$ ev)
      -- where
      --   nn = elClass "span" "sidenav-text" $ text $ untag (n ^. title)
