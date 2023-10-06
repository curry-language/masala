module Controller.Registration ( registrationController, registrationForm, generateValidationToken ) where

import Data.Time
import HTML.Base
import HTML.Session
import HTML.WUI
import Model.Masala2
import Model.Queries
import Config.EntityRoutes
import Config.UserProcesses
import Config.Roles
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
import Database.CDBI.Connection
import Crypto.Hash

type RegistrationInput =
    ( String -- LoginName
    , String -- PublicName
    , String -- Email
    , String -- Password
    , String -- Password (Repetition)
    )

--- Shows a form to register a new User.
registrationController :: Controller
registrationController =
  checkAuthorization (userOperationAllowed NewEntity)
   $ (\sinfo ->
     do setParWuiStore registrationStore sinfo ("","","","","")
        return [formElem registrationForm])

--- The data stored for executing the "registration" WUI form.
registrationStore
  :: SessionStore (UserSessionInfo, WuiStore RegistrationInput)
registrationStore = sessionStore "registrationStore"

--- A WUI form to create register a new User.
--- The default values for the fields are stored in 'registrationStore'.
registrationForm
  :: HtmlFormDef (UserSessionInfo, WuiStore RegistrationInput)
registrationForm =
  pwui2FormDef "Controller.Registration.registrationForm" registrationStore
   (\_ -> wRegistration)
   (\_ (loginName, publicName, email, uncryptpasswd, _) ->
     checkAuthorization (userOperationAllowed NewEntity)
      (\_ -> do
        cryptpasswd <- getUserHash loginName uncryptpasswd
        usernameAvailable <- checkUserNameAvailable loginName
        publicnameAvailable <- checkPublicNameAvailable publicName
        emailAvailable <- checkEmailAvailable email
        if usernameAvailable && publicnameAvailable && emailAvailable
          then do
            userResult <- registerUser loginName publicName email cryptpasswd
            case userResult of 
              Left err -> displayError $
                "Controller.Registration: Adding new user did not work, " ++
                "although input data was correct (error: " ++ show err ++ ")"
              Right user -> do
                time <- getClockTime
                tokenResult <- generateValidationToken time (userKey user)
                case tokenResult of 
                  Left err -> displayError $
                    "Controller.Registration: Adding new token did not work, " ++
                    "although token should be free (error: " ++ show err ++ ")"
                  Right token -> sendValidationMail email (validationTokenToken token)
          else displayRegistrationError usernameAvailable publicnameAvailable
                                        emailAvailable))
   (\sinfo -> renderWUIWithText sinfo "Register to Masala" "Register"
                                [par [htxt explain]] "?Registration")
 where
  explain = unlines
    [ "You can create a new account in order to upload your packages to Masala."
    , "You need to provide a valid email address which is used to verify"
    , "your account, sending lost passwords, etc."
    , "This email address is not shown to other users of Masala."
    , "You must also choose a public name which is shown in Masala"
    , "with your uploaded packages."
    , "Later you can also decide to add a public email address to your profile."
    , "In this case, this email address will be shown to other users"
    , "if they want to contact you."
    ]

--- This function registers a user by creating a new user entity in the database.
registerUser :: String -> String -> String -> String -> IO (SQLResult User)
registerUser loginName publicName email cryptpasswd = runT $
  newUser loginName publicName email "" roleInvalid cryptpasswd "" Nothing

--- Shows a error message for a user registration.
displayRegistrationError :: Bool -> Bool -> Bool -> Controller
displayRegistrationError usernameAvailable publicnameAvailable emailAvailable =
  let err1 = errorUsernameAvailable usernameAvailable
      err2 = errorPublicnameAvailable publicnameAvailable
      err3 = errorEmailAvailable emailAvailable
  in displayError $ unlines $ filter (not . null) [err1, err2, err3]
  where
    errorUsernameAvailable True  = ""
    errorUsernameAvailable False =
      "The given login name is not available, please choose another one."

    errorPublicnameAvailable True  = ""
    errorPublicnameAvailable False =
      "The given public user name is not available, please choose another one."

    errorEmailAvailable True  = ""
    errorEmailAvailable False =
      "The given email address is already used, please choose another one."


--- This function generates a random validation token and adds it to the database.
generateValidationToken :: ClockTime -> UserID -> IO (SQLResult ValidationToken)
generateValidationToken time user = do
  token <- randomString 32
  available <- checkValidationTokenAvailable token
  if available
    then runT (newValidationTokenWithUserValidatingKey token time user)
    else generateValidationToken time user
