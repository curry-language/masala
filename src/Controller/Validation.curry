module Controller.Validation ( validationController, validationForm ) where

import Data.Time
import HTML.Base
import HTML.Session
import HTML.WUI
import Model.Masala2
import Model.Queries
import Config.Masala (invalidTime)
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
--- Deletes too old validation token as a side effect
--- before looking for the given validation token.
validationController :: Controller
validationController = do
  deleteOldValidationToken
  args <- getControllerParams
  case args of 
    [] -> validationTokenController
    [token] -> do 
      result <- getValidationTokenWithToken token
      case result of
        Nothing -> displayError $ errorNoToken token
        Just validationToken -> do
          deleteValidationTokenWithToken token
          validationResult <- validateUser
                             (validationTokenUserValidatingKey validationToken)
          case validationResult of 
            Left err -> displayError $ "Validation failed (" ++ show err ++ ")"
            Right _ -> do
              setPageMessage
                "Your new Masala account has been successfully validated"
              redirectController "?"
    _ -> displayUrlError
 where
  errorNoToken token =
    "This validation token \"" ++ token ++ "\" does not exist. " ++
    "If you have already registered, you can request a new validation " ++
    "token using \"Validate your account\" in Masala's registration menu."

  validateUser :: UserID -> IO (SQLResult ())
  validateUser user = runT $ validateUserAction user

  validateUserAction :: UserID -> DBAction ()
  validateUserAction key = do 
    oldUser <- getUser key
    updateUser (setUserRole oldUser roleNotTrusted)


--- Shows a form to edit the given User entity.
validationTokenController :: Controller
validationTokenController = do
  sinfo <- getUserSessionInfo
  setParWuiStore validationStore sinfo ""
  return [formElem validationForm]

--- A WUI form to issue a new ValidationToken for a User.
--- The default values for the fields are stored in 'validationStore'.
validationForm :: HtmlFormDef (UserSessionInfo, WuiStore String)
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
                                    setPageMessage $ "Something went wrong, please try again (" ++
                                                     show err ++ ")"
                                    redirectController "?Validation"
                                Right newToken -> do 
                                    sendValidationMail (userEmail user) (validationTokenToken newToken)
                        else do
                            setPageMessage "User is already validated"
                            redirectController "?Validation"
        )
        (\sinfo->
            renderWUIWithText sinfo "Validate:" "Send validation token"
              [par [htxt explain]] "?"
        )
 where
  explain = "If you have already registered with a name and email address " ++
            "but forgot to validate it, you can request " ++
            "a new validation token which will be sent to your registered " ++
            "email address."

--- The data stored for executing the "validation" WUI form.
validationStore :: SessionStore (UserSessionInfo,WuiStore String)
validationStore = sessionStore "validationStore"