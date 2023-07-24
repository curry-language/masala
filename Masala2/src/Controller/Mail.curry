--- A controller to send emails and informs the result in a web page

module Controller.Mail
  ( mailController, adminMailController, mailFormDef
  , sendMail, sendValidationMail, sendPasswordMail )
 where

import Control.Monad  ( unless )
import Data.Time
import System.IOExts  ( exclusiveIO )
import qualified System.Mail

import Config.Masala
import HTML.Base
import HTML.Session
import System.Authentication
import System.Spicey
import View.Mail

------------------------------------------------------------------------------
--- Controller for login/logout.
mailController :: Controller
mailController = do
  args <- getControllerParams
  case args of
    ["admin"] -> adminMailController "" ""
    _         -> displayUrlError

------------------------------------------------------------------------------
adminMailController :: String -> String -> Controller
adminMailController subject contents = do
  mblogin <- getSessionLoginName
  case mblogin of 
    Nothing    -> displayError "You must be logged in to send emails"
    Just login -> do
      let cmt = "With this form you can contact the administrator of Masala " ++
                "by email, e.g., to request the publication of a package " ++
                "or to change your user status."
      putSessionData mailViewData
        (login,cmt,"the Masala administrator",adminEmail,subject,contents)
      return $ [formElem mailFormDef]

mailFormDef :: HtmlFormDef (String,String,String,String,String,String)
mailFormDef = formDefWithID "Controller.Mail.mailFormDef"
  (getSessionData mailViewData ("","","","","",""))
  (mailView sendMailController)

--- The session store for the mail view contains the login name,
--- the comment shown at the top, the name of the recipient,
--- the mail address of the recipient, an initial subject and contents.
mailViewData :: SessionStore (String,String,String,String,String,String)
mailViewData = sessionStore "mailViewData"

------------------------------------------------------------------------------

--- Sends an email similarly to `System.Mail.sendMail` and log the
--- email in the email log file of Masala.
--- @param from - the email address of the sender
--- @param to - the email address of the recipient
--- @param subject - the subject of the email
--- @param contents - the contents of the email
sendMail :: String -> String -> String -> String -> IO ()
sendMail from to subject contents = do
  let mailtxt = System.Mail.showSendMail adminEmail to subject contents
      line    = take 78 (repeat '-')
  ctime <- fmap (calendarTimeToString . toUTCTime) getClockTime
  exclusiveIO (emailLogFile ++ ".LOCK") $
    appendFile emailLogFile
               (line ++ "\nSent at " ++ ctime ++ " (UTC)\n" ++ mailtxt)
  unless testSystem $ System.Mail.sendMail from to subject contents

--- A controller which sends an email to a given recipient with
--- given subject and contents. After sending, the info document
--- provided as the first argument is shown.
sendMailController :: [BaseHtml] -> String -> String -> String -> Controller
sendMailController emailInfo to subject contents = do
  let mailtxt = System.Mail.showSendMail adminEmail to subject contents
  sendMail adminEmail to subject contents
  if testSystem
    then return $ emailInfo ++
                  -- for testing only
                  [ hrule
                  , h3 [htxt "Mail sent in the real system:"]
                  , verbatim mailtxt ]
    else return emailInfo

sendPasswordMail :: String -> String -> Controller
sendPasswordMail to password = do
  let subject = "Your password has been reset"
      contents = "Your password has been reset and replaced with '" ++
                 password ++ "'.\n" ++
                 "Please use it to login and change your password again."
      emailInfo = [ h3 [htxt "Password reset successful!"],
                    par [htxt $ "In order to login, please use the password contained " ++
                                "in an email sent to '" ++ to ++ "'."] ]
  sendMailController emailInfo to subject contents

-- Send an email to request the validation of the registered email address.
sendValidationMail :: String -> String -> Controller
sendValidationMail to token = do 
  let subject  = "Please validate your email address"
      contents = "To validate your email address, please go to the URL\n"++
                 baseURL ++ "?Validation/" ++ token
      emailInfo = [ h3 [htxt "Initial registration successful!"],
                    par [htxt $
                      "In order to use your account, please activate it by " ++
                      "following the link (URL) containing in an email " ++
                      "sent to '" ++ to ++ "'." ] ]
  sendMailController emailInfo to subject contents
{-
sendValidationMail :: String -> String -> Controller
sendValidationMail to token = do
  let subject  = "Please validate your email address"
      contents = "To validate your email address, please go to the URL "++
                 baseURL ++ "?Validation/" ++ token
  -- for testing:
  let mailtxt = showSendMail adminEmail to subject contents
  return $ emailInfo to ++
           -- for testing only:
           [ hrule,
             h3 [htxt "Mail sent in the real system:"],
             verbatim mailtxt ]
 where
  emailInfo emailaddr =
    [ h3 [htxt "Initial registration successful!"],
      par [htxt $ "In order to use your account, please activate it by " ++
                  "it by following the link (URL) containing in an email " ++
                  "sent to '" ++ emailaddr ++ "'." ] ]
-}

------------------------------------------------------------------------------
