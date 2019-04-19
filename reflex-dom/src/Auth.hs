{-# LANGUAGE
    RecursiveDo
  , LambdaCase
  , TypeFamilies
#-}

module Auth
  ( login
  , forgotPassword
  , register
  , resetPassword
  , checkAuth
  , destroyAccessToken
  ) where

-----
import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( decode, encode )
import Data.String ( String )
import qualified Data.ByteString.Lazy.Char8 as L ( pack, unpack )
import GHCJS.DOM ( currentWindow )
import GHCJS.DOM.Window ( getSessionStorage )
import GHCJS.DOM.Storage ( getItem, removeItem, setItem )
import GHCJS.DOM.Types ( MonadDOM )
import Lens.Micro.Platform ( (^.) )
import Reflex.Dom
-----
import CGX.Prelude hiding ( id )
import Api.Routes
import Api.Xhr
import Models.Auth
import Models.Lenses
import Models.Types
import qualified Support.Dom.Inputs as I
import qualified Support.Dom.FFI as F
import Support.Legos
import Support.Legos.Flash
import qualified Support.Legos.Materialize as M
import Support.Session
import Support.Util
import Support.Routes
-----

login :: MonadWidget t m
  => JunksState
  -> m (Event t JunksState)
login ua = do
  ev <- lag
  mTkn <- loadAccessToken
  case mTkn of
    Just tkn -> checkingToken tkn (setView (VNewNote Nothing) Nothing ua) ev
    _ -> auth ua

auth :: MonadWidget t m
  => JunksState
  -> m (Event t JunksState)
auth ua =
  M.elTitleCardContainer "Log in" $ do
    rec
      case ua ^. trigger of
        Just TRegistered -> el "p" $ text "Thanks for registering. You can log in using the form below."
        Just TPasswordReset -> el "p" $ text "Password reset. You can log in using the form below."
        _ -> blank

      _ <- el "div" $ flashWidgetEv flashConfig ((whenError . decodeAuthResponse) evResp)

      (_, evEmailEnter, dyEmail) <- I.email "login_email" (I.TabIndex "1") never
      (_, evPasswordEnter, dyPassword) <- I.password "login_password" (I.TabIndex "2") never
      el "p" $ text "By using this tool you agree to four things."
      el "ol" $ do
        el "li" $ text "You understand what \"Browser Cookies\" are."
        el "li" $ text "You understand what \"Use at your own risk.\" means."
        el "li" $ text "You understand what \"We are not liable for any and all damages cause by use of this tool.\" means."
        el "li" $ text "This tool uses Browser Cookies and you will use this tool at your own risk and we are not liable for any and all damages caused by using this tool."
      evClick <- divClass "center-align row" $ M.elButton (M.BtnDefault M.None) "Log In"
      evForgotPassword <- divClass "center-align row" $ elTextLink "I forgot my password."
      evRegister <- divClass "center-align row" $ elTextLink "Create a free account."
      _ <- F.setFocus "login_email"

      evResp <-
        apiRoute
        (Login (AuthRequest <$> dyEmail <*> dyPassword))
        Nothing
        $ leftmost [evEmailEnter, evPasswordEnter, evClick]

      let evAuthed =
            setView (VNewNote Nothing) (Just TLoggedIn) .
            authedSession ua <$>
            whenRight (decodeAuthResponse evResp)

      _ <- storeAccessToken . extractToken . whenRight $ decodeAuthResponse evResp

    return $ leftmost
      [ setView VRegister Nothing ua <$ evRegister,
        setView VForgotPassword Nothing ua <$ evForgotPassword,
        evAuthed
      ]
  where
    extractToken = fmap (^. token)

register :: MonadWidget t m
  => JunksState
  -> m (Event t JunksState)
register ua =
  M.elTitleCardContainer "Create Free Account" $ do
    rec
      _ <- el "div" $ flashWidgetEv flashConfig (whenError evDec)

      (_, evEnter1, dyEmail) <- I.email "email" (I.TabIndex "1") never
      (_, evEnter2, dyUsername) <- I.username "name" (I.TabIndex "2") never
      (_, evEnter3, dyPassword0) <- I.password "password0" (I.TabIndex "3") never
      (_, evEnter4, dyPassword1) <- I.passwordC "password1" (I.TabIndex "4") never
      evClick <- divClass "center-align row" $ M.elButton (M.BtnDefault M.None) "Sign Up"
      evLogIn <- divClass "center-align row" $ elTextLink "I already have an account."
      _ <- F.setFocus "email"

      evResp <-
        apiRoute
        (Register (RegisterRequest <$> dyEmail <*> dyUsername <*> dyPassword0 <*> dyPassword1))
        Nothing
        $ leftmost [evEnter1, evEnter2, evEnter3, evEnter4, evClick]

      let evDec = decodeEmpty evResp

    return $ leftmost
      [ setView VLogIn Nothing ua <$ evLogIn
      , setView VLogIn (Just TRegistered) ua <$ whenRight evDec
      ]

forgotPassword :: MonadWidget t m
  => JunksState
  -> m (Event t JunksState)
forgotPassword ua =
  M.elTitleCardContainer "Password Reset Request" $
    case ua ^. trigger of
      Just TPasswordReset -> do
        el "p" $ text "Check your email for password reset instructions."
        evOK <- divClass "center-align row" $ M.elButton (M.BtnDefault M.None) "OK"
        return $ setView VLogIn Nothing ua <$ evOK
      _ -> do
        rec
          _ <- el "div" $ flashWidgetEv flashConfig (whenError evDec)

          (_, evEnter, dyEmail) <- I.email "email" (I.TabIndex "1") never
          evClick <- divClass "center-align row" $ M.elButton (M.BtnDefault M.None) "Send Recovery Email"
          evLogIn <- divClass "center-align row" $ elTextLink "I remember my password."
          _ <- F.setFocus "email"

          evResp <-
            apiRoute
            (ForgotPW (ForgotPWRequest <$> dyEmail))
            Nothing
            $ leftmost [evEnter, evClick]

          let evDec = decodeEmpty evResp

        return $ leftmost
          [ setView VLogIn Nothing ua <$ evLogIn
          , setView VForgotPassword (Just TPasswordReset) ua <$ whenRight evDec
          ]

resetPassword :: MonadWidget t m
  => ResetToken
  ->JunksState
  -> m (Event t JunksState)
resetPassword tkn ua =
  M.elTitleCardContainer "Password Reset" $ do
    rec
      _ <- el "div" $ flashWidgetEv flashConfig (whenError evDec)

      (_, evEnter1, dyPassword0) <- I.password "password0" (I.TabIndex "1") never
      (_, evEnter2, dyPassword1) <- I.passwordC "password1" (I.TabIndex "2") never
      evClick <- divClass "center-align row" $ M.elButton (M.BtnDefault M.None) "Reset Password"
      evLogIn <- divClass "center-align row" $ elTextLink "I don't want to reset my password."
      _ <- F.setFocus "password0"

      evResp <-
        apiRoute
        (ResetPW (ResetPWRequest <$> constDyn tkn <*> dyPassword0 <*> dyPassword1))
        Nothing
        $ leftmost [evEnter1, evEnter2, evClick]

      let evDec = decodeEmpty evResp
    return $ leftmost
      [ setView VLogIn Nothing ua <$ evLogIn
      , setView VLogIn (Just TPasswordReset) ua <$ whenRight evDec
      ]

checkAuth :: MonadWidget t m
  => JunksState
  -> Event t a
  -> m (Event t JunksState)
checkAuth ua t = do
  mTkn <- loadAccessToken
  case mTkn of
    Just tkn -> checkingToken tkn ua t
    _ -> noAuth

-- | Get the access token (authorization) from the current session, if applicable
loadAccessToken :: MonadDOM m => m (Maybe AuthToken)
loadAccessToken = do
  Just window <- currentWindow
  sessionStorage <- getSessionStorage window
  fetchT <- getItem sessionStorage tokenKey
  return $
    case fetchT of
      Nothing -> Nothing
      Just s -> (decode . L.pack) s :: Maybe AuthToken
  where
    tokenKey = "access_token" :: String

noAuth :: MonadWidget t m => m (Event t JunksState)
noAuth = do
  ev <- lag
  return $ defaultJS <$ ev

checkingToken :: MonadWidget t m
  => AuthToken
  -> JunksState
  -> Event t a
  -> m (Event t JunksState)
checkingToken tkn ua ev = do
  _ <- waiting
  evReq <- apiRoute Reauth (Just tkn) (() <$ ev)
  let evDec = fforMaybe (decodeAuthResponse evReq) $ \case
                Left _ -> Nothing
                Right ar -> Just $ authedSession ua ar

  -- If evAuth failed then wipe out the access token
  evTknDead <- destroyAccessToken . whenLeft $ decodeAuthResponse evReq
  _ <- storeAccessToken . extractToken . whenRight $ decodeAuthResponse evReq

  return $ leftmost [defaultJS <$ evTknDead, evDec]
  where
    extractToken = fmap (^. token)

waiting :: MonadWidget t m => m (Event t JunksState)
waiting =
  M.elCardContainer $ do
    divClass "center-align" $ el "h1" $ text "Trying to remember you..."
    return never

storeAccessToken :: MonadWidget t m
  => Event t AuthToken
  -> m (Event t ())
storeAccessToken = perform storeToken
  where
    storeToken t = do
      Just window <- currentWindow
      sessionStorage <- getSessionStorage window
      setItem sessionStorage keyT (L.unpack . encode $ t)
    keyT = "access_token" :: String

destroyAccessToken :: MonadWidget t m
  => Event t a
  -> m (Event t ())
destroyAccessToken = perform dropToken
  where
    dropToken _ = do
      Just window <- currentWindow
      sessionStorage <- getSessionStorage window
      removeItem sessionStorage keyT
    keyT = "access_token" :: String

perform :: MonadWidget t m
  => (b -> IO a)
  -> Event t b
  -> m (Event t a)
perform f x = performEvent (fmap (liftIO . f) x)
