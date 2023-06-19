--------------------------------------------------------------------------
--- This module implements the views related to the standard controllers
--- in a Spicey application.
--- In particular, it defines a default view for login
--- and a view of a list of user processes.
--------------------------------------------------------------------------

module View.SpiceySystem
  ( loginView, processListView, historyView )
 where

import HTML.Base
import HTML.Styles.Bootstrap4 ( hrefScndSmButton, primSmButton, scndButton )

import Config.UserProcesses
import System.Processes
import System.Spicey
import System.Authentication
import Model.Queries

-----------------------------------------------------------------------------
--- Generates a form for login/logout.
--- If the passed login name is the empty string,
--- we offer a login dialog, otherwise a logout dialog.
loginView :: (Maybe String, String) -> [HtmlExp]
loginView (currlogin,lurl) =
  case currlogin of
   Nothing -> [h3 [htxt "Login as:"],
               htxt "Login name:", nbsp, textField loginfield "", nbsp,
               htxt "Password:", nbsp, password passwdfield, nbsp,
               primSmButton "Login" loginHandler]
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
    loginSuccess <- checkLoginData loginname cryptpasswd
    if loginSuccess
      then do role <- getRoleOfUser loginname
              loginToSession loginname role
              setPageMessage $
                "Logged in as: " ++ loginname ++ " / role: " ++ role
      else return ()
    nextInProcessOr (redirectController "?") Nothing >>= getPage

  logoutHandler _ = do
    logoutFromSession >> setPageMessage "Logged out"
    nextInProcessOr (redirectController "?") Nothing >>= getPage

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
