{-# LANGUAGE
    DataKinds
  , FlexibleContexts
  , GADTs
  , RankNTypes
  , ScopedTypeVariables
  , TypeOperators
#-}

module Api.Routes where

import Data.Proxy
import Data.Map ( insert )
import Lens.Micro.Platform ( over )
import Reflex.Dom
import Servant.API
import Servant.Reflex

import CGX.Prelude
import Models.Auth
import Models.Content
import Models.Types

buildHeader :: AuthToken -> Text
buildHeader t
  = "Bearer " <> untag t

attachHeader :: AuthToken -> XhrRequest a -> XhrRequest a
attachHeader t r =
  let l = xhrRequest_config . xhrRequestConfig_headers
      f = insert "Authorization" (buildHeader t)
  in over l f r

apiRoute ::
  forall t m. SupportsServantReflex t m
  => ApiRoute t
  -> Maybe AuthToken
  -> Event t ()
  -> m (Event t (Maybe XhrResponse))
apiRoute route tkn trig = do
  let base = "/api"
      authedRequest = ClientOptions $ \r -> return $
        case tkn of
          Just t -> attachHeader t r
          Nothing -> r

  let (     login
       :<|> register
       :<|> forgotPW
       :<|> resetPW
       )=
       client (Proxy :: Proxy AuthApi)
              (Proxy :: Proxy m)
              (Proxy :: Proxy ())
              (constDyn (BasePath base))

  let (
            reauth
       )=
       clientWithOpts
         (Proxy :: Proxy UserApi)
         (Proxy :: Proxy m)
         (Proxy :: Proxy ())
         (constDyn (BasePath base))
         authedRequest

  let (
            getGroups
       :<|> newGroup
       :<|> newTag
       :<|> deleteTag
       )=
       clientWithOpts
         (Proxy :: Proxy GroupApi)
         (Proxy :: Proxy m)
         (Proxy :: Proxy ())
         (constDyn (BasePath base))
         authedRequest

  let (
            newNote
       :<|> getNote
       :<|> updateNote
       :<|> attachTag
       :<|> detachTag
       )=
       clientWithOpts
         (Proxy :: Proxy NoteApi)
         (Proxy :: Proxy m)
         (Proxy :: Proxy ())
         (constDyn (BasePath base))
         authedRequest

  case route of
    Login a -> login (fmr a) trig >>= return . fmap response
    Register a -> register (fmr a) trig >>= return . fmap response
    ForgotPW a -> forgotPW (fmr a) trig >>= return . fmap response
    ResetPW a -> resetPW (fmr a) trig >>= return . fmap response

    Reauth -> reauth trig >>= return . fmap response
    Groups -> getGroups trig >>= return . fmap response
    NewGroup p -> newGroup (fmr p) trig >>= return .fmap response
    NewTag g t -> newTag (fmr g) (fmr t) trig >>= return . fmap response
    DeleteTag g t -> deleteTag (fmr g) (fmr t) trig >>= return . fmap response

    GetNote i -> getNote (fmr i) trig >>= return . fmap response
    NewNote p -> newNote (fmr p) trig >>= return . fmap response
    UpdateNote i p -> updateNote (fmr i) (fmr p) trig >>= return .fmap response
    AttachTag n t -> attachTag (fmr n) (fmr t) trig >>= return . fmap response
    DetachTag n t -> detachTag (fmr n) (fmr t) trig >>= return . fmap response

    NotFound -> return $ Nothing <$ trig
  where
    -- Servant Relfex allows for preprocessing.
    -- You can return a Left Text to indicate something is wrong with the
    -- payload and you don't want to send it along.
    -- Because of the way we're leveraging types this is not necessary
    -- at this time.
    fmr = fmap Right
    -- fmq = fmap QParamSome -- for query params
    -- jt a = Just <$> a -- wrap a Just
    -- no a = Nothing <$ a -- wrap a Nothing

----

data ApiRoute t =
  -- NoAuthUserApi
    Login (Dynamic t AuthRequest)
  | Register (Dynamic t RegisterRequest)
  | ForgotPW (Dynamic t ForgotPWRequest)
  | ResetPW (Dynamic t ResetPWRequest)
  -- UserApi
  | Reauth
  -- GroupApi
  | Groups
  | NewGroup (Dynamic t NewGroupRequest)
  | NewTag (Dynamic t GroupId) (Dynamic t NewTagRequest)
  | DeleteTag (Dynamic t GroupId) (Dynamic t TagId)
  -- NoteApi
  | NewNote (Dynamic t NewNoteRequest)
  | GetNote (Dynamic t NoteId)
  | UpdateNote (Dynamic t NoteId) (Dynamic t UpdateNoteRequest)
  | AttachTag (Dynamic t NoteId) (Dynamic t TagId)
  | DetachTag (Dynamic t NoteId) (Dynamic t TagId)
  -- Not Found
  | NotFound

--- Public Endpoints ---
--- Auth ---

type AuthApi =
  "user" :> (
       LoginApi
  :<|> RegisterApi
  :<|> ForgotPWApi
  :<|> ResetPWApi
  )

type LoginApi =
  "login" :>
  ReqBody '[JSON] AuthRequest :>
  Post '[JSON] AuthResponse

type RegisterApi =
  "register" :>
  ReqBody '[JSON] RegisterRequest :>
  Post '[JSON] NoContent

type ForgotPWApi =
  "forgot" :>
  ReqBody '[JSON] ForgotPWRequest :>
  Post '[JSON] NoContent

type ResetPWApi =
  "reset" :>
  ReqBody '[JSON] ResetPWRequest :>
  Post '[JSON] NoContent

--- Authed Endpoints ---
--- User ---

type UserApi =
  "user" :> (
       ReauthApi
  )

type ReauthApi =
  "reauth" :>
  Post '[JSON] AuthResponse

--- Group ---

type GroupApi =
  "groups" :> (
       GetGroupsApi
  :<|> NewGroupApi
  :<|> NewTagApi
  :<|> DeleteTagApi
  )

type GetGroupsApi =
  Get '[JSON] [GroupShallow]

type NewGroupApi =
  ReqBody '[JSON] NewGroupRequest :>
  Post '[JSON] GroupResponse

type NewTagApi =
  Capture "group_id" GroupId :>
  ReqBody '[JSON] NewTagRequest :>
  Post '[JSON] TagResponse

type DeleteTagApi =
  Capture "group_id" GroupId :>
  Capture "tag_id" TagId :>
  Delete '[JSON] TagResponse

--- Note ---

type NoteApi =
  "notes" :> (
       NewNoteApi
  :<|> GetNoteApi
  :<|> UpdateNoteApi
  :<|> AttachTagApi
  :<|> DetachTagApi
  )

type NewNoteApi =
  ReqBody '[JSON] NewNoteRequest :>
  Post '[JSON] NoteResponse

type GetNoteApi =
  Capture "note_id" NoteId :>
  Get '[JSON] NoteResponse

type UpdateNoteApi =
  Capture "note_id" NoteId :>
  ReqBody '[JSON] UpdateNoteRequest :>
  Post '[JSON] NoteResponse

type AttachTagApi =
  Capture "note_id" NoteId :>
  "tags" :>
  Capture "tag_id" TagId :>
  Post '[JSON] TagResponse

type DetachTagApi =
  Capture "note_id" NoteId :>
  "tags" :>
  Capture "tag_id" TagId :>
  Delete '[JSON] TagResponse
