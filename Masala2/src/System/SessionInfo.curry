----------------------------------------------------------------------------
--- This module defines the data that is associated to a user session
--- and passed to a view so that views can be adapted to user sessions.
---
--- Currently, the session data contains information about the login
--- status of a user. Typically, this information will be extended
--- according to the concrete application.
---
--- @author Michael Hanus
----------------------------------------------------------------------------

module System.SessionInfo (
  UserSessionInfo(..), userLoginOfSession, setUserLoginOfSession, 
  getUserSessionInfo, updateUserSessionInfo
 ) where

import HTML.Base    ( fromFormReader )
import HTML.Session

--------------------------------------------------------------------------
--- The data associated to a user session.
--- It contains formation about the login status of a user.
--- The argument of the session data is `Nothing` if the user is not logged in.
--- Otherwise, it is `Maybe ln` where `ln` is the login name of the user.
data UserSessionInfo = SD (Maybe String)
  deriving (Read,Show)

--- The initial (empty) session data
emptySessionInfo :: UserSessionInfo
emptySessionInfo = SD Nothing

--- Extracts the login status from the user session data.
userLoginOfSession :: UserSessionInfo -> Maybe String
userLoginOfSession (SD login) = login

--- Sets the login status of the user session data.
setUserLoginOfSession :: Maybe String -> UserSessionInfo -> UserSessionInfo
setUserLoginOfSession login (SD _) = SD login

--------------------------------------------------------------------------
--- Definition of the session state to store the login name (as a string).
userSessionInfo :: SessionStore UserSessionInfo
userSessionInfo = sessionStore "userSessionInfo"

--- Gets the data of the current user session.
getUserSessionInfo :: IO UserSessionInfo
getUserSessionInfo =
  fromFormReader $ getSessionData userSessionInfo emptySessionInfo

--- Updates the data of the current user session.
updateUserSessionInfo :: (UserSessionInfo -> UserSessionInfo) -> IO ()
updateUserSessionInfo = modifySessionData userSessionInfo emptySessionInfo

--------------------------------------------------------------------------
