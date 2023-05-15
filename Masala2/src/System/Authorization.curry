--- This module specifies the access authorization to web pages.

module System.Authorization
  ( AccessType(..), AccessResult(..), checkAuthorization )
 where

import HTML.Base
import System.Spicey(Controller,displayError)
import System.SessionInfo

--- The various kinds of operations applied to an entity.
data AccessType a = NewEntity | ShowEntity a | UpdateEntity a | DeleteEntity a
                  | ListEntities

--- The result of checking an authorization. The access is either granted
--- or denied with a string explaining the reason.
data AccessResult = AccessGranted | AccessDenied String

--- Checks the results of an authoriation access.
--- If the access is granted, we proceed with the given controller
--- to which the current user session information is passed,
--- otherwise we display the access error message.
checkAuthorization :: (UserSessionInfo -> IO AccessResult)
                   -> (UserSessionInfo -> Controller)
                   -> Controller
checkAuthorization getaccess controller = do
  sinfo <- getUserSessionInfo
  accresult <- getaccess sinfo
  case accresult of
    AccessGranted       -> controller sinfo
    AccessDenied reason -> displayError reason
