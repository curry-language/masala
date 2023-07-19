--------------------------------------------------------------------------
--- This module implements the views related to the standard controllers
--- in a Spicey application.
--- In particular, it defines a default view for login
--- and a view of a list of user processes.
--------------------------------------------------------------------------

module View.Mail ( mailView )
 where

import HTML.Base
import HTML.Styles.Bootstrap4 ( hrefScndSmButton, primSmButton, scndButton )
import HTML.WUI

import Config.Masala
import System.Spicey

-----------------------------------------------------------------------------
--- A generic form to send emails.
mailView :: ([BaseHtml] -> String -> String -> String -> Controller)
              -> (String,String,String,String) -> [HtmlExp]
mailView sendmail (login,mailcmt,toname,tomail) =
  [ h3 [htxt $ "Send mail to " ++ toname ]
  , par [htxt mailcmt]
  , par [htxt "Subject: ", nbsp,
         textField subjectfield "" `addAttr` ("size","50")]
  , textArea contentsfield (10,60) "", nbsp
  , primSmButton "Send" sendHandler]
 where 
  subjectfield, contentsfield free

  sendHandler env = do
    let subject = "[Masala: from " ++ login ++ "] " ++ env subjectfield
    sendmail [ h3 [htxt "Email has been sent."]]
             tomail subject (env contentsfield) >>= getPage

-----------------------------------------------------------------------------
