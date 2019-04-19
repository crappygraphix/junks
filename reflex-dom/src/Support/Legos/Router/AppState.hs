{-|
  Module      : Support.Legos.Router.AppState
  Description : Application state management.
-}
module Support.Legos.Router.AppState where

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
import CGX.Prelude
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-|
  AppState typeclass to facilitate SPA URL addresses.

  'defaultState' constructs the SPA's initial state.
  'toUrl' takes a state and returns a tupple where fst is the URL as Text, and snd is state.
  'fromUrl' takes a tupple where fst is a list of url parts where "a/b/c" would be provided as ["a","b","c"],
  snd is the current state, and returns a new state.

  Below is an example of a minimally implemented AppState.

  data MyState
    = MyState
    { myStatePage     :: Page
    , myStateMetaData :: Maybe MetaData
    }
  makeLensesWith camelCaseFields ''MyState

  instance AppState MyState where
    defaultState = MyState PLogIn Nothing
    toUrl ms = (pageToUrl (ms ^. page), ms)
    fromUrl (parts, ms) = set page (urlPartsToPage parts) ms

  urlPartsToPage :: [Text] -> Page
  urlPartsToPage ts =
    case ts of
      [""]          -> PLogIn
      ["login"]     -> PLogIn
      ["dashboard"] -> PDashboard
      ["admin"]     -> PAdmin
      x             -> P404 (intercalate "/" x)

  pageToUrl :: Page -> Text
  pageToUrl v =
    case v of
      PLogIn     -> "login"
      PDashboard -> "dashboard"
      PAdmin     -> "admin"
      _          -> "invalid_page"

-}
class AppState s where
  defaultState :: s
  toUrl :: s -> (Text, s)
  fromUrl :: ([Text], s) -> s
