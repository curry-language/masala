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

-----------------------------------------------------------------------------
--- Generates a form for login/logout.
--- If the passed login name is the empty string,
--- we offer a login dialog, otherwise a logout dialog.
loginView :: (Maybe String, String) -> [HtmlExp]
loginView (currlogin,lurl) =
  case currlogin of
   Nothing -> [h3 [htxt "Login as:"],
               textField loginfield "", nbsp,
               primSmButton "Login" loginHandler]
   Just _  -> [h3 [htxt "Really logout?"],
               primSmButton "Logout" logoutHandler, nbsp,
               hrefScndSmButton lasturl [htxt "Cancel"]]
 where
  loginfield free

  lasturl = '?' : lurl

  loginHandler env = do
    let loginname = env loginfield
    -- In the real system, you should also verify a password here.
    if null loginname
      then return ()
      else do loginToSession loginname
              setPageMessage ("Logged in as: " ++ loginname)
    nextInProcessOr (redirectController lasturl) Nothing >>= getPage

  logoutHandler _ = do
    logoutFromSession >> setPageMessage "Logged out"
    nextInProcessOr (redirectController lasturl) Nothing >>= getPage

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
