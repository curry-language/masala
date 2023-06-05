module System.AuthorizedActions where

import System.Authentication ( getSessionLogin )
import System.Authorization
import System.SessionInfo
import Model.Masala2

----------------------------------------------------------------------------
--- Grants access for the administrator.
checkAdmin :: UserSessionInfo -> IO AccessResult
checkAdmin sinfo =
  return $ if isAdminSession sinfo
             then AccessGranted
             else AccessDenied "Operation only allowed for admin!"

--- Grants access if somebody is logged in.
isLoggedIn :: IO AccessResult
isLoggedIn =
  getSessionLogin >>=
  return . maybe (AccessDenied "Operation not allowed!") (const AccessGranted)

----------------------------------------------------------------------------
--- Checks whether the application of an operation to a User
--- entity is allowed.
userOperationAllowed :: AccessType User -> UserSessionInfo -> IO AccessResult
userOperationAllowed at sinfo =
  case at of
    ListEntities -> isLoggedIn
    NewEntity    -> return AccessGranted
    ShowEntity _ -> return AccessGranted
    _            -> checkAdmin sinfo

--- Checks whether the application of an operation to a Package
--- entity is allowed.
packageOperationAllowed
  :: AccessType Package -> UserSessionInfo -> IO AccessResult
packageOperationAllowed at sinfo =
  case at of
    ListEntities -> return AccessGranted
    ShowEntity _ -> return AccessGranted
    _            -> checkAdmin sinfo

--- Checks whether the application of an operation to a Version
--- entity is allowed.
versionOperationAllowed
  :: AccessType Version -> UserSessionInfo -> IO AccessResult
versionOperationAllowed at sinfo =
  case at of
    ListEntities -> return AccessGranted
    ShowEntity _ -> return AccessGranted
    _            -> checkAdmin sinfo

--- Checks whether the application of an operation to a Category
--- entity is allowed.
categoryOperationAllowed
  :: AccessType Category -> UserSessionInfo -> IO AccessResult
categoryOperationAllowed at sinfo =
  case at of
    ListEntities -> return AccessGranted
    ShowEntity _ -> return AccessGranted
    _            -> checkAdmin sinfo

--- Checks whether the application of an operation to a CurryModule
--- entity is allowed.
curryModuleOperationAllowed
  :: AccessType CurryModule -> UserSessionInfo -> IO AccessResult
curryModuleOperationAllowed at sinfo =
  case at of
    ListEntities -> return AccessGranted
    ShowEntity _ -> return AccessGranted
    _            -> checkAdmin sinfo

--- Checks whether the application of an operation to a ValidationToken
--- entity is allowed.
validationTokenOperationAllowed
  :: AccessType ValidationToken -> UserSessionInfo -> IO AccessResult
validationTokenOperationAllowed at _ =
  case at of
    ListEntities -> return AccessGranted
    NewEntity -> return AccessGranted
    ShowEntity _ -> return AccessGranted
    DeleteEntity _ -> return AccessGranted
    UpdateEntity _ -> return AccessGranted
