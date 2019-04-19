{-# LANGUAGE
    FlexibleInstances
  , FlexibleContexts
  , FunctionalDependencies
  , MultiParamTypeClasses
  , TemplateHaskell
  , RankNTypes
#-}

module Support.Session where

-----
import Lens.Micro.Platform hiding ( view )
import Reflex
import Reflex.Dom
import Servant.Reflex
-----
import CGX.Prelude
import Api.Routes
import Models.Auth
import Support.Legos.Router
import Models.Lenses
import Models.Types
import Support.Routes
-----

data Session
  = Session
  { sessionAuthToken :: Maybe AuthToken
  , sessionAuthedUser :: Maybe User
  } deriving (Show, Eq)
makeLensesWith camelCaseFields ''Session

newSession :: Session
newSession = Session Nothing Nothing

data JunksState
  = JunksState
  { junksStateView :: View
  , junksStateSession :: Session
  , junksStateTrigger :: Maybe Trigger
  }
makeLensesWith camelCaseFields ''JunksState

instance AppState JunksState where
  defaultState = defaultJS
  toUrl ua = (viewToUrl (ua ^. view), ua)
  fromUrl (parts, ua) = set view (urlPartsToView parts) ua

defaultJS :: JunksState
defaultJS = JunksState VLogIn newSession Nothing

logOutJS :: JunksState
logOutJS = JunksState VLogIn newSession (Just TLoggedOut)

mkJunksState :: View -> Session -> Maybe Trigger -> JunksState
mkJunksState = JunksState

setView :: View -> Maybe Trigger -> JunksState -> JunksState
setView v t = set view v . set trigger t

authedSession :: JunksState -> AuthResponse -> JunksState
authedSession ua r =
  set trigger (Just TLoggedIn) . set session (Session (Just $ r ^. token) (Just $ r ^. user)) $ ua

callApi ::
  forall t m a. SupportsServantReflex t m
  => ApiRoute t
  -> JunksState
  -> Event t a
  -> m (Event t (Maybe XhrResponse))
callApi r u ev = apiRoute r (u ^. session . authToken) (() <$ ev)
