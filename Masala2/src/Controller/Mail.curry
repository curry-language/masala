--- A controller to send emails and informs the result in a web page

module Controller.Mail ( sendValidationMail ) where

import System.Mail

import Config.Masala
import HTML.Base
import System.Spicey

-- Send an email to request the validation of the registered email address.
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
