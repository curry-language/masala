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
  getSessionLogin, loginToSession, logoutFromSession
 ) where

import System.SessionInfo
import Crypto.Hash

--------------------------------------------------------------------------
-- Operations for hashing.

--- Gets a hash string for a user name and password. The generated
--- hash string should be stored for the user instead of the password.
getUserHash :: String -> String -> IO String
getUserHash username userpassword = do
  let systemkey = "3spicey5" -- change this key for every spicey instance
  getHash (username++userpassword++systemkey)

--- Returns a random password (a hexadecimal string) of a particular length.
--- @param length - length of the desired password
--- @return the random password
randomPassword :: Int -> IO String
randomPassword = randomString


--------------------------------------------------------------------------
-- Operations for storing logins in the current session.

--- Gets the login name of the current session
--- (or the Nothing if there is no login).
getSessionLogin :: IO (Maybe String)
getSessionLogin = fmap userLoginOfSession getUserSessionInfo

--- Stores a login name in the current session.
--- The authentication has to be done before!
loginToSession :: String -> IO ()
loginToSession loginname =
  updateUserSessionInfo (setUserLoginOfSession (Just loginname))

--- removes the login name from the current session.
logoutFromSession :: IO ()
logoutFromSession = updateUserSessionInfo (setUserLoginOfSession Nothing)

--------------------------------------------------------------------------
