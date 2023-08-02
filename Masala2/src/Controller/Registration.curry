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

--- The data stored for executing the "new entity" WUI form.
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

registerUser :: String -> String -> String -> String -> IO (SQLResult User)
registerUser loginName publicName email cryptpasswd = runT $
  newUser loginName publicName email "" roleInvalid cryptpasswd "" Nothing

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

generateValidationToken :: ClockTime -> UserID -> IO (SQLResult ValidationToken)
generateValidationToken time user = do
  token <- randomString 32
  available <- checkValidationTokenAvailable token
  if available
    then runT (newValidationTokenWithUserValidatingKey token time user)
    else generateValidationToken time user

{-
--- Transaction to persist a new User entity to the database.
registrationT :: RegistrationInput -> DBAction User
registrationT (loginName,publicName,email,cryptpasswd) =
    newUser loginName publicName email "" "Invalid" cryptpasswd "" Nothing

checkIfUsernameAvailable :: Connection -> String -> IO Bool
checkIfUsernameAvailable connection username = do
  result <- runDBAction queryAllUsers connection
  case result of 
    Left err -> error "checkIfUsernameAvailable: queryAllUsers failed"
    Right users -> return $ all (notUser username) users
  where
    notUser :: String -> User -> Bool
    notUser username user = userLoginName user /= username

checkEmailSyntax :: String -> Bool
checkEmailSyntax email = True

checkIfEmailAvailable :: Connection -> String -> IO Bool
checkIfEmailAvailable connection email = do 
  result <- runDBAction queryAllUsers connection
  case result of 
    Left err -> error "checkIfEmailAvailable: queryAllUsers failed"
    Right users -> return $ all (notEmail email) users
  where
    notEmail :: String -> User -> Bool
    notEmail email user = userEmail user /= email

checkIfPasswordFine :: String -> Bool
checkIfPasswordFine = checkPasswordLength

checkPasswordLength :: String -> Bool
checkPasswordLength uncryptpasswd = length uncryptpasswd >= 8
-}