module Support.Routes where

-----
import Data.Text
import Models.Types
import Text.Read ( read )
-----
import CGX.Prelude
-----

data View
  = VLogIn
  | VRegister
  | VForgotPassword
  | VResetPassword ResetToken

  | VNewGroup
  | VGroupSettings GroupId
  | VNewNote (Maybe GroupId)
  | VNote NoteId
  | VEditNote NoteId

  | V404
  deriving (Show, Eq)

data Trigger
  = TLoggedIn
  | TLoggedOut
  | TRegistered
  | TPasswordReset
  | TRefreshNav
  deriving (Show, Eq)

urlPartsToView :: [Text] -> View
urlPartsToView ts =
  case ts of
    [""]                          -> VLogIn
    ["login"]                     -> VLogIn
    ["register"]                  -> VRegister
    ["forgot"]                    -> VForgotPassword
    ["reset", tkn]                -> VResetPassword (mk tkn)
    ["group", "new"]              -> VNewGroup
    ["note", "new"]               -> VNewNote Nothing
    ["group", gid, "note", "new"] -> VNewNote (Just (mk . t2i $ gid))
    ["group", gid, "settings"]    -> VGroupSettings (mk . t2i $ gid)
    ["note", nid]                 -> VNote (mk . t2i $ nid)
    ["note", nid, "edit"]         -> VEditNote (mk . t2i $ nid)
    _                             -> V404
  where
    t2i = read . unpack

viewToUrl :: View -> Text
viewToUrl v =
  case v of
    VLogIn              -> "login"
    VRegister           -> "register"
    VForgotPassword     -> "forgot"
    VResetPassword tkn  -> "reset/" <> untag tkn
    VNewGroup           -> "group/new"
    VNewNote Nothing    -> "note/new"
    VNewNote (Just gid) -> "group/" <> (i2t . untag $ gid) <> "/note/new"
    VNote nid           -> "note/" <> (i2t . untag $ nid)
    VEditNote nid       -> "note/" <> (i2t . untag $ nid) <> "/edit"
    VGroupSettings gid  -> "group/" <> (i2t . untag $ gid) <> "/settings"
    _                   -> "404"
  where
    i2t = pack . show
