module Controller.Registration ( registrationController, registrationForm ) where

import Data.Time
import HTML.Base
import HTML.Session
import HTML.WUI
import Model.Masala2
import Model.Queries
import Config.EntityRoutes
import Config.UserProcesses
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
    )

--- Shows a form to register a new User.
registrationController :: Controller
registrationController =
  checkAuthorization (userOperationAllowed NewEntity)
   $ (\sinfo ->
     do setParWuiStore registrationStore sinfo ("","","","")
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
   (\_ (loginName, publicName, email, uncryptpasswd) ->
     checkAuthorization (userOperationAllowed NewEntity)
      (\_ -> do
        cryptpasswd <- getUserHash loginName uncryptpasswd
        usernameAvailable <- checkUserNameAvailable loginName
        let emailSyntax = checkEmailSyntax email
        emailAvailable <- checkEmailAvailable email
        let passwordFine = checkIfPasswordFine uncryptpasswd
        let check = usernameAvailable && emailSyntax && emailAvailable && passwordFine

        if check
          then do
            time <- getClockTime
            token <- generateValidationToken
            connection <- connectSQLite sqliteDBFile
            userResult <- runDBAction (newUser loginName publicName email "" "Invalid" cryptpasswd "" Nothing) connection
            case userResult of 
              Left err -> do 
                disconnect connection
                displayError "Controller.Registration: Adding new user did not work, although input data was correct."
              Right user -> do 
                tokenResult <- runDBAction (newValidationTokenWithUserValidatingKey token time (userKey user)) connection
                disconnect connection
                case tokenResult of 
                  Left err -> displayError "Controller.Registration: Adding new token did not work, although token should be free."
                  Right validationToken ->
                    sendValidationMail email token
                --addValidationToken token time (userKey user)
                --validationToken <- getValidationTokenWithToken token
                 
            {-
            transactionControllerWith
                 (runT
                    (registrationT (loginName, publicName, email, cryptpasswd)))
                 (\user -> nextInProcessOr (redirectController (showRoute user))
                                           Nothing)
                                          -}
          else displayRegistrationError usernameAvailable emailSyntax emailAvailable passwordFine))
   (\sinfo ->
     renderWUI sinfo "Register new User" "Register" "?Registration" ())

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

displayRegistrationError :: Bool -> Bool -> Bool -> Bool -> Controller
displayRegistrationError usernameAvailable emailSyntax emailAvailable passwordFine =
  let err1 = errorUsernameAvailable usernameAvailable
      err2 = errorEmailSyntax emailSyntax
      err3 = errorEmailAvailable emailAvailable
      err4 = errorPasswordFine passwordFine
  in displayError (err1 ++ err2 ++ err3 ++ err4)
  where
    errorUsernameAvailable True = ""
    errorUsernameAvailable False = "The given username is not available, please choose another one.\n"

    errorEmailSyntax True = ""
    errorEmailSyntax False = "The given email address is not a correct email address, please make sure to give a correct email address.\n"

    errorEmailAvailable True = ""
    errorEmailAvailable False = "The given email address is already used, please choose another one.\n"

    errorPasswordFine True = ""
    errorPasswordFine False = "The password must be at least 8 symbols long.\n"

generateValidationToken :: IO String
generateValidationToken = do
  token <- randomString 32
  available <- checkValidationTokenAvailable token
  if available
    then return token
    else generateValidationToken