--------------------------------------------------------------------------
--- This module implements the views related to the standard controllers
--- in a Spicey application.
--- In particular, it defines a default view for login
--- and a view of a list of user processes.
--------------------------------------------------------------------------

module View.Validation
  ( wValidation )
 where

import HTML.WUI
import HTML.Base
import System.Spicey
import View.EntitiesToHtml

wValidation :: WuiSpec String -- LoginName or Email
wValidation =
  withRendering
   wRequiredString
   (renderLabels loginNameEmailLabelList)
