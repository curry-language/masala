--------------------------------------------------------------------------
--- This module contains some controller that might be used in in
--- Spicey application.
--- In particular, it provides a controller for login/out and
--- a controller to start selected user processes.
--------------------------------------------------------------------------

module Controller.SpiceySystem
  ( loginController, loginFormDef
  , processListController, historyController
  )
 where

import Numeric

import Config.UserProcesses
import System.Spicey
import HTML.Base
import HTML.Session
import System.Processes
import System.Authentication
import View.SpiceySystem

-----------------------------------------------------------------------------
--- Controller for login/logout.
loginController :: Controller
loginController = do
  login <- getSessionLogin
  putSessionData loginViewData login
  return [formElem loginFormDef]

loginFormDef :: HtmlFormDef (Maybe String)
loginFormDef = formDefWithID "Controller.SpiceySystem.loginFormDef"
  (getSessionData loginViewData Nothing) loginView

--- The data processed by the login form.
loginViewData :: SessionStore (Maybe String)
loginViewData = sessionStore "loginViewData"

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
