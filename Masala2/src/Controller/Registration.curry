module Controller.Registration ( registrationController, registrationForm ) where

import Data.Time
import HTML.Base
import HTML.Session
import HTML.WUI
import Masala2
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
        connection <- connectSQLite sqliteDBFile

        cryptpasswd <- getUserHash loginName uncryptpasswd
        usernameAvailable <- checkIfUsernameAvailable connection loginName
        let emailSyntax = checkEmailSyntax email
        emailAvailable <- checkIfEmailAvailable connection email
        let passwordFine = checkIfPasswordFine uncryptpasswd
        let check = usernameAvailable && emailSyntax && emailAvailable && passwordFine

        disconnect connection

        transactionController (runT (registrationT check (loginName, publicName, email, cryptpasswd)))
         (nextInProcessOr (redirectController "?Registration") Nothing)))
   (\sinfo ->
     renderWUI sinfo "Register new User" "Register" "?Registration" ())

--- Transaction to persist a new User entity to the database.
registrationT :: Bool -> RegistrationInput -> DBAction ()
registrationT False _ = return ()
registrationT True (loginName,publicName,email,cryptpasswd) =
    newUser loginName publicName email "" "Invalid" cryptpasswd "" Nothing
    >> return ()

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
checkIfPasswordFine uncryptpasswd = True