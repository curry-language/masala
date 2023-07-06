module Controller.Upload
  ( uploadController, uploadFormDef ) where

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
    sinfo <- getUserSessionInfo
    -- CHECK AUTHORIZATION
    case userLoginOfSession sinfo of 
        Nothing -> displayError "You must be logged in to upload packages"
        Just (loginName, role) -> do
            putSessionData uploadViewData loginName
            return $ [formElem uploadFormDef]

uploadFormDef :: HtmlFormDef String
uploadFormDef = formDefWithID "Controller.Upload.uploadFormDef"
    (getSessionData uploadViewData "") uploadView

uploadViewData :: SessionStore String
uploadViewData = sessionStore "uploadViewData"