module Controller.Upload
  ( uploadController, uploadFormDef, uploadByName ) where

import Data.List ( last, sortBy )

import Data.Maybe
import Data.Time
import HTML.Base
import HTML.Session
import HTML.WUI
import Model.Masala2
import Model.Queries
import Config.EntityRoutes
import Config.Roles
import Config.UserProcesses
import System.SessionInfo
import System.Authorization
import System.AuthorizedActions
import System.PackageHelpers
import System.Spicey
import View.EntitiesToHtml
import View.Upload
import Database.CDBI.Connection


uploadController :: Controller
uploadController = do
    checkAuthorization
        (packageOperationAllowed NewEntity)
        (\sinfo ->
            case userLoginOfSession sinfo of 
                Nothing -> displayError "You must be logged in to upload packages"
                Just (loginName, role) -> do
                    putSessionData uploadViewData loginName
                    return $ [formElem uploadFormDef]
        )

uploadFormDef :: HtmlFormDef String
uploadFormDef = formDefWithID "Controller.Upload.uploadFormDef"
    (getSessionData uploadViewData "") uploadView

uploadViewData :: SessionStore String
uploadViewData = sessionStore "uploadViewData"

--- Uploads a package in batch mode, i.e., the parameters are the
--- login name, the encrypted password and the text of the package
--- specification (`package.json` file contents.
--- Then it is checked whether the password is correct and the user
--- is allowed to upload the package.
--- The result is either a message about the successful upload
--- or an error message.
uploadByName :: String -> String -> String -> IO String
uploadByName login passwd packagetxt = do
  -- an empty implementation just returning the parameters:
  let answer = unlines ["LOGIN: " ++ login, "PASSWORD: " ++ passwd
                       ,"PACKAGE:", packagetxt]
  return answer

