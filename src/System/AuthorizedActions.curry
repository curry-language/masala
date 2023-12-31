module System.AuthorizedActions where

import System.Authentication ( getSessionLogin )
import System.Authorization
import System.SessionInfo
import Model.Masala2
import Model.Queries

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
    ListEntities -> checkAdmin sinfo
    NewEntity    -> return AccessGranted
    ShowEntity _ -> return AccessGranted
    UpdateEntity user -> adminOrLoggedInAsUser user sinfo
    _            -> checkAdmin sinfo


--- Checks whether the current user is either an admin or the given User.
adminOrLoggedInAsUser :: User -> UserSessionInfo -> IO AccessResult
adminOrLoggedInAsUser user sinfo =
  return $ if isAdminSession sinfo || loggedInAsUserSession user sinfo 
             then AccessGranted
             else AccessDenied "Operation not allowed!"

--- Checks whether the application of an operation to a Package
--- entity is allowed.
packageOperationAllowed
  :: AccessType Package -> UserSessionInfo -> IO AccessResult
packageOperationAllowed at sinfo =
  case at of
    ListEntities -> return AccessGranted
    ShowEntity _ -> return AccessGranted
    UpdateEntity package -> adminOrMaintainer package sinfo
    NewEntity    -> isLoggedIn
    _            -> checkAdmin sinfo

--- Checks whether the current user is an admin or a maintainer of the given Package.
adminOrMaintainer :: Package -> UserSessionInfo -> IO AccessResult
adminOrMaintainer package sinfo =
  if isAdminSession sinfo
    then return AccessGranted
    else do
      case userLoginOfSession sinfo of 
        Nothing -> operationDenied
        Just (loginName, _) -> do 
          userResult <- getUserByName loginName
          case userResult of 
            Nothing-> operationDenied
            Just user -> do 
              isMaintainer <- checkIfMaintainer package user
              case isMaintainer of 
                False -> operationDenied
                True -> return AccessGranted

--- Checks whether the application of an operation to a Version
--- entity is allowed.
versionOperationAllowed
  :: AccessType Version -> UserSessionInfo -> IO AccessResult
versionOperationAllowed at sinfo = case at of
  ListEntities -> return AccessGranted
  ShowEntity v ->
    if versionPublished v || isAdminSession sinfo
      then return AccessGranted
      else case userLoginOfSession sinfo of
              Nothing -> showDenied
              Just (login,_) -> do
                mbuser <- getUserByName login
                case mbuser of
                  Nothing -> showDenied
                  Just user -> do
                    pkg <- runQ $ getVersioningPackage v
                    isMaintainer <- checkIfMaintainer pkg user
                    case isMaintainer of
                      False -> showDenied
                      True  -> return AccessGranted
  _            -> checkAdmin sinfo
 where
  showDenied = return $ AccessDenied "Version is not yet published"

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
validationTokenOperationAllowed at sinfo =
  case at of
    _            -> checkAdmin sinfo

operationDenied :: IO AccessResult
operationDenied = return $ AccessDenied "Operation not allowed!"
