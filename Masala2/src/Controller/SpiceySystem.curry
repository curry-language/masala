--------------------------------------------------------------------------
--- This module contains some controller that might be used in in
--- Spicey application.
--- In particular, it provides a controller for login/out and
--- a controller to start selected user processes.
--------------------------------------------------------------------------

module Controller.SpiceySystem
  ( loginController, loginFormDef
  , processListController, historyController
  , forgotPasswordController, forgotPasswordForm
  )
 where

import Numeric

import Config.UserProcesses
import Controller.Mail
import System.Spicey
import HTML.Base
import HTML.Session
import HTML.WUI
import Model.Masala2
import Model.Queries
import System.Processes
import System.Authentication
import View.SpiceySystem

-----------------------------------------------------------------------------
--- Controller for login/logout.
loginController :: Controller
loginController = do
  login <- getSessionLoginName
  lasturl <- getLastUrl
  putSessionData loginViewData (login, lasturl)
  args <- getControllerParams
  case args of 
    [] -> return [formElem loginFormDef]
    ["forgotpassword"] -> forgotPasswordController
    _ -> displayUrlError

loginFormDef :: HtmlFormDef (Maybe String, String)
loginFormDef = formDefWithID "Controller.SpiceySystem.loginFormDef"
  (getSessionData loginViewData (Nothing,"")) loginView

--- The data processed by the login form.
loginViewData :: SessionStore (Maybe String, String)
loginViewData = sessionStore "loginViewData"

forgotPasswordController :: Controller
forgotPasswordController = do
  setParWuiStore forgotPasswordStore () ""
  return [formElem forgotPasswordForm]

forgotPasswordStore
  :: SessionStore ((), WuiStore String)
forgotPasswordStore = sessionStore "forgotPasswordStore"

forgotPasswordForm
  :: HtmlFormDef ((), WuiStore String)
forgotPasswordForm =
  pwui2FormDef "Controller.SpiceySystem.forgotPasswordForm" forgotPasswordStore
    (\_ -> wForgotPassword)
    (\_ login -> do
      userResult <- getUserByNameOrEmail login
      case userResult of 
        Nothing -> do 
          setPageMessage "User with that name/email address does not exist"
          redirectController "?login/forgotpassword"
        Just user -> do 
          uncryptpasswd <- randomPassword 16
          cryptpasswd <- getUserHash (userLoginName user) uncryptpasswd
          updateUserResult <- runT (updateUser $ setUserPassword user cryptpasswd)
          case updateUserResult of 
            Left err -> do 
              setPageMessage "Something went wront, please try again"
              redirectController "?/forgotpassword"
            Right _ -> do
              sendPasswordMail (userEmail user) uncryptpasswd
    )
    (\sinfo ->
      renderWUIWithText sinfo "Reset and send a new password"
        "Reset password" [par [htxt explain]] "?")
 where
  explain = "If you forgot your password, a new password can be created " ++
            "and sent to your registered email address. " ++
            "For this purpose, type in below your login name or " ++
            "email address."

-----------------------------------------------------------------------------
--- Controller for showing and selecting user processes.
processListController :: Controller
processListController = do
  args <- getControllerParams
  if null args
    then return $ processListView availableProcesses
    else case readInt (head args) of
           [(idInt, "")] ->
             startProcess (processNames availableProcesses !! (idInt - 1))
           _              -> displayError "could not read process id"

-----------------------------------------------------------------------------
--- Controller for the URL history.
historyController :: Controller
historyController = getLastUrls >>= return . historyView

-----------------------------------------------------------------------------
