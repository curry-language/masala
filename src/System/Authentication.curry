----------------------------------------------------------------------------
--- This library contains operations to support the management of
--- user authentication. It provides operations for password
--- generation, password hashing (based on hashing algorithms from Unix),
--- and storing logins in sessions.
---
--- @author Michael Hanus
----------------------------------------------------------------------------

module System.Authentication (
  getUserHash, randomPassword,
  getSessionLogin, getSessionLoginName, loginToSession, logoutFromSession,
  isAdmin
 ) where

import Config.Masala ( systemHashKey )
import System.SessionInfo
import Crypto.Hash

--------------------------------------------------------------------------
-- Operations for hashing.

--- Gets a hash string for a user name and password. The generated
--- hash string should be stored for the user instead of the password.
getUserHash :: String -> String -> IO String
getUserHash username userpassword = do
  getHash (username ++ userpassword ++ systemHashKey)

--- Returns a random password (a hexadecimal string) of a particular length.
--- @param length - length of the desired password
--- @return the random password
randomPassword :: Int -> IO String
randomPassword = randomString


--------------------------------------------------------------------------
-- Operations for storing logins in the current session.

--- Gets the login info of the current session
--- (or the Nothing if there is no login).
getSessionLogin :: IO (Maybe (String,String))
getSessionLogin = fmap userLoginOfSession getUserSessionInfo

--- Gets the login name of the current session
--- (or the Nothing if there is no login).
getSessionLoginName :: IO (Maybe String)
getSessionLoginName = fmap (maybe Nothing (Just . fst)) getSessionLogin

--- Stores a login name in the current session.
--- The authentication has to be done before!
loginToSession :: String -> String -> IO ()
loginToSession loginname role =
  updateUserSessionInfo (setUserLoginOfSession (Just (loginname, role)))

--- removes the login name from the current session.
logoutFromSession :: IO ()
logoutFromSession = updateUserSessionInfo (setUserLoginOfSession Nothing)

--------------------------------------------------------------------------
-- Returns true if admin is logged in.
isAdmin :: IO Bool
isAdmin = getUserSessionInfo >>= return . isAdminSession

--------------------------------------------------------------------------
