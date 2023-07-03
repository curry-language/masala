module Controller.Validation ( validationController, validationForm ) where

import Data.Time
import HTML.Base
import HTML.Session
import HTML.WUI
import Model.Masala2
import Model.Queries
import Config.EntityRoutes
import Config.UserProcesses
import Config.Roles
import Controller.Registration
import Controller.Mail
import System.SessionInfo
import System.Authentication
import System.Authorization
import System.AuthorizedActions
import System.Spicey
import System.PreludeHelpers
import View.EntitiesToHtml
import View.User
import View.Registration
import View.Validation
import Database.CDBI.Connection

--- Shows a form to register a new User.
validationController :: Controller
validationController = do
    args <- getControllerParams
    case args of 
        [] -> validationTokenController
        [token] -> do 
            result <- getValidationTokenWithToken token
            case result of
                Nothing -> displayError ("This validation token \"" ++ token ++ "\" does not exist.")
                Just validationToken -> do
                    deleteValidationTokenWithToken token
                    validationResult <- validateUser (validationTokenUserValidatingKey validationToken)
                    case validationResult of 
                        Left err -> displayError "Validation failed"
                        Right _ -> do
                            setPageMessage "Successfully validated"
                            redirectController "?"
        _ -> displayUrlError

validateUser :: UserID -> IO (SQLResult ())
validateUser user = do
    runT (validateUserAction user)
    where
        validateUserAction key = do 
            oldUser <- getUser key
            updateUser (setUserRole oldUser roleNotTrusted)


--- Shows a form to edit the given User entity.
validationTokenController :: Controller
validationTokenController = do
    setParWuiStore validationStore () ""
    return [formElem validationForm]

validationForm
  :: HtmlFormDef ((),WuiStore String)
validationForm =
    pwui2FormDef "Controller.Validation.validationForm" validationStore
        (\_ -> wValidation)
        (\_ login -> do
            userResult <- getUserByNameOrEmail login
            case userResult of 
                Nothing -> do
                    setPageMessage "User with that name/email address does not exist"
                    redirectController "?Validation"
                Just user -> do
                    if userRole user == roleInvalid
                        then do
                            time <- getClockTime
                            currentTokenResult <- getUserValidationToken user
                            newTokenResult <- case currentTokenResult of 
                                Nothing -> generateValidationToken time (userKey user)
                                Just currentToken -> do 
                                    let newToken = setValidationTokenValidSince currentToken time
                                    runT (updateValidationToken newToken >+ return newToken)
                            case newTokenResult of
                                Left err -> do 
                                    setPageMessage "Something went wrong, please try again"
                                    redirectController "?Validation"
                                Right newToken -> do 
                                    sendValidationMail (userEmail user) (validationTokenToken newToken)
                        else do
                            setPageMessage "User is already validated"
                            redirectController "?Validation"
        )
        (\_ ->
            renderWUIWithText () "Validate:" "Send validation token"
              [par [htxt explain]] "?"
        )
 where
  explain = "If you have already registered with a name and email address " ++
            "but forgot to validate it, you can request " ++
            "a new validation token which will be send to your registered " ++
            "email address:"

validationStore
  :: SessionStore ((),WuiStore String)
validationStore = sessionStore "validationStore"