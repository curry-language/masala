--------------------------------------------------------------------------
--- This module implements the views related to the standard controllers
--- in a Spicey application.
--- In particular, it defines a default view for login
--- and a view of a list of user processes.
--------------------------------------------------------------------------

module View.SpiceySystem
  ( loginView, processListView, historyView, wForgotPassword )
 where

import Data.Time
import HTML.Base
import HTML.Styles.Bootstrap4 ( hrefScndSmButton, primSmButton, scndButton )
import HTML.WUI

import Config.UserProcesses
import System.Processes
import System.Spicey
import System.Authentication
import Model.Masala2
import Model.Queries
import View.EntitiesToHtml

-----------------------------------------------------------------------------
--- Generates a form for login/logout.
--- If the passed login name is the empty string,
--- we offer a login dialog, otherwise a logout dialog.
loginView :: (Maybe String, String) -> [HtmlExp]
loginView (currlogin,lurl) =
  case currlogin of
   Nothing -> [h3 [htxt "Login as:"],
               htxt "Login name:", nbsp, 
               textField loginfield "" `addAttr` ("autofocus",""), nbsp,
               htxt "Password:", nbsp, password passwdfield, nbsp,
               primSmButton "Login" loginHandler, nbsp,
               primSmButton "Forgot Password?" forgottenPasswordHandler]
   Just _  -> [h3 [htxt "Really logout?"],
               primSmButton "Logout" logoutHandler, nbsp,
               hrefScndSmButton lasturl [htxt "Cancel"]]
 where
  loginfield,passwdfield free

  lasturl = '?' : lurl

  loginHandler env = do
    let loginname = env loginfield
        passwdtxt = env passwdfield
    -- In the real system, you should also verify a password here.
    cryptpasswd <- getUserHash loginname passwdtxt
    userResult <- getUserByLoginData loginname cryptpasswd
    case userResult of
      Just user -> do
        let role = userRole user
        loginToSession loginname role
        ctime <- getClockTime
        runT (updateUser (setUserLastLogin user (Just ctime)))
        setPageMessage $
          "Logged in as: " ++ loginname ++ " / role: " ++ role
        redirectController lasturl >>= getPage
      Nothing -> do
        setPageMessage $
          "Either login name or password were not correct or you are " ++
          "not yet validated, please try again"
        redirectController lasturl >>= getPage
    --nextInProcessOr (redirectController "?") Nothing >>= getPage

  logoutHandler _ = do
    logoutFromSession >> setPageMessage "Logged out"
    nextInProcessOr (redirectController lasturl) Nothing >>= getPage
  
  forgottenPasswordHandler _ = do 
    redirectController "?login/forgotpassword" >>= getPage

wForgotPassword :: WuiSpec String
wForgotPassword =
  withRendering
    wRequiredString
    (renderLabels loginNameEmailLabelList)

-----------------------------------------------------------------------------
--- A view for all processes contained in a given process specification.
processListView :: Processes a -> [BaseHtml]
processListView procs =
  [h1 [htxt "Processes"],
   ulist (map processColumn (zip (processNames procs) [1..]))]
 where
   processColumn (pname, id) =
     [href ("?spiceyProcesses/" ++ show id) [htxt pname]]

-----------------------------------------------------------------------------
--- A view for all URLs of a session.
historyView :: [String] -> [BaseHtml]
historyView urls =
  [h1 [htxt "History"],
   ulist (map (\url -> [href ("?" ++ url) [htxt url]])
              (filter (not . null) urls))]

-----------------------------------------------------------------------------
