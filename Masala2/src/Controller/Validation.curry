module Controller.Validation ( validationController ) where

import Data.Time
import HTML.Base
import HTML.Session
import HTML.WUI
import Model.Masala2
import Model.Queries
import Config.EntityRoutes
import Config.UserProcesses
import System.SessionInfo
import System.Authentication
import System.Authorization
import System.AuthorizedActions
import System.Spicey
import System.PreludeHelpers
import View.EntitiesToHtml
import View.User
import View.Registration
import Database.CDBI.Connection

--- Shows a form to register a new User.
validationController :: Controller
validationController = do
    args <- getControllerParams
    case args of 
        [token] -> do 
            result <- getValidationTokenWithToken token
            case result of
                Nothing -> displayError ("This validation token \"" ++ token ++ "\" does not exist.")
                Just validationToken -> do
                    deleteValidationTokenWithToken token
                    validateUser (validationTokenUserValidatingKey validationToken)
                    displayError "User is validated."
        _ -> displayUrlError
