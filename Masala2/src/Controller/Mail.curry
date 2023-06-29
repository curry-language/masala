--- A controller to send emails and informs the result in a web page

module Controller.Mail ( sendValidationMail, sendPasswordMail ) where

import System.Mail

import Config.Masala
import HTML.Base
import System.Spicey

sendMail :: [BaseHtml] -> String -> String -> String -> Controller
sendMail emailInfo to subject contents = do
  let mailtxt = showSendMail adminEmail to subject contents
  return $ emailInfo ++
            -- for testing only
            [ hrule,
             h3 [htxt "Mail sent in the real system:"],
             verbatim mailtxt ]

sendPasswordMail :: String -> String -> Controller
sendPasswordMail to password = do
  let subject = "Your password has been reset"
      contents = "Your password has been reset and replaced with '"
                 ++ password
                 ++ "'. Please use it to login and change your password again."
      emailInfo = [ h3 [htxt "Password reset successful!"],
                    par [htxt $ "In order to login, please use the password contained " ++
                                "in an email sent to '" ++ to ++ "'."] ]
  sendMail emailInfo to subject contents

-- Send an email to request the validation of the registered email address.
sendValidationMail :: String -> String -> Controller
sendValidationMail to token = do 
  let subject  = "Please validate your email address"
      contents = "To validate your email address, please go to the URL "++
                 baseURL ++ "?Validation/" ++ token
      emailInfo = [ h3 [htxt "Initial registration successful!"],
                    par [htxt $ "In order to use your account, please activate it by " ++
                                "it by following the link (URL) containing in an email " ++
                                "sent to '" ++ to ++ "'." ] ]
  sendMail emailInfo to subject contents
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
