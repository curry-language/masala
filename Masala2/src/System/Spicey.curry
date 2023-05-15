--------------------------------------------------------------------------
--- This module implements some auxiliary operations to support the
--- generic implementation of the Spicey entities.
--------------------------------------------------------------------------

module System.Spicey (
  Controller, EntityController(..), showRoute, editRoute, deleteRoute,
  applyControllerOn,
  redirectController,
  nextController, nextControllerForData,
  confirmDeletionPage,
  transactionController,
  getControllerURL,getControllerParams, showControllerURL,
  getPage, wDateType, wBoolean, wUncheckMaybe, wFloat,
  displayError, displayUrlError, cancelOperation,
  renderWUI, renderLabels,
  nextInProcessOr,
  stringToHtml, maybeStringToHtml,
  intToHtml,maybeIntToHtml, floatToHtml, maybeFloatToHtml,
  boolToHtml, maybeBoolToHtml, dateToHtml, maybeDateToHtml,
  userDefinedToHtml, maybeUserDefinedToHtml,
  spTable,
  setPageMessage, getPageMessage,
  saveLastUrl, getLastUrl, getLastUrls
  ) where

import Data.Char        ( isSpace, isDigit )

import Data.Time
import System.FilePath     ( (</>) )

import Database.CDBI.Connection ( SQLResult )
import HTML.Base
import HTML.Session
import HTML.Styles.Bootstrap4
import HTML.WUI

import Config.UserProcesses
import System.Routes
import System.Processes
import System.Authentication

--------------------------------------------------------------------------
-- a viewable can be turned into a representation which can be displayed
-- as interface
-- here: a representation of a HTML page
type Viewable = HtmlPage

type ViewBlock = [BaseHtml]

--- Controllers contains all logic and their result should be a Viewable.
--- if the behavior of controller should depend on URL parameters
--- (following the first name specifying the controller), one
--- can access these URL parameters by using the operation
--- Spicey.getControllerParams inside the controller.
type Controller = IO ViewBlock


--- The type class `EntityController` contains:
--- * the application of a controller to some entity identified by a key string
--- * an operation to construct a URL route for an entity w.r.t. to a route
---   string
class EntityController a where
  controllerOnKey :: String -> (a -> Controller) -> Controller

  entityRoute :: String -> a -> String


--- Returns the URL route to show a given entity.
showRoute :: EntityController a => a -> String
showRoute = entityRoute "show"

--- Returns the URL route to edit a given entity.
editRoute :: EntityController a => a -> String
editRoute = entityRoute "edit"

--- Returns the URL route to delete a given entity.
deleteRoute :: EntityController a => a -> String
deleteRoute = entityRoute "delete"


--- Reads an entity for a given key and applies a controller to it.
applyControllerOn :: Maybe enkey -> (enkey -> IO en)
                  -> (en -> Controller) -> Controller
applyControllerOn Nothing _ _ = displayUrlError
applyControllerOn (Just userkey) getuser usercontroller =
  getuser userkey >>= usercontroller

--- A controller to redirect to an URL starting with "?"
--- (see implementation of `getPage`).
redirectController :: String -> Controller
redirectController url = return [htmlText url]

nextController :: Controller -> _ -> IO HtmlPage
nextController controller _ = do
  view <- controller
  getPage view

-- for WUIs
nextControllerForData :: (a -> Controller) -> a -> IO HtmlPage
nextControllerForData controller param = do
  view <- controller param
  getPage view


--- Generates a page to ask the user for a confirmation to delete an entity
--- specified in the controller URL (of the form "entity/delete/key/...").
--- The yes/no answers are references derived from the controller URL
--- where the second argument is replaced by "destroy"/"show".
--- @param question - a question asked
--- @param yescontroller - the controller used if the answer is "yes"
--- @param nocontroller  - the controller used if the answer is "no"
confirmDeletionPage :: UserSessionInfo -> String -> Controller
confirmDeletionPage _ question = do
  (entity,ctrlargs) <- getControllerURL
  case ctrlargs of
    (_:args) -> return $
      [h3 [htxt question],
       par [hrefPrimSmButton (showControllerURL entity ("destroy":args))
                             [htxt "Yes"],
            nbsp,
            hrefScndSmButton (showControllerURL entity ["list"]) [htxt "No"]]]
    _ -> displayUrlError


--- A controller to execute a transaction and proceed with a given
--- controller if the transaction succeeds. Otherwise, the
--- transaction error is shown.
--- @param trans - the transaction to be executed
--- @param controller - the controller executed in case of success
transactionController :: IO (SQLResult _) -> Controller -> Controller
transactionController trans controller = do
  transResult <- trans
  either (\error -> displayError (show error))
         (\_     -> controller)
         transResult

--- If we are in a process, execute the next process depending on
--- the provided information passed in the second argument,
--- otherwise execute the given controller (first argument).
nextInProcessOr :: Controller -> Maybe ControllerResult -> Controller
nextInProcessOr controller arg = do
  isproc <- isInProcess
  if isproc then advanceInProcess arg >> return [htxt ""] -- triggers redirect
            else controller


--------------------------------------------------------------------------
-- Operations for handling URL parameters

--- Parse the URL parameter passed to the main script. The result is a pair
--- consisting of the route and the list of parameters separated by '/'.
parseUrl :: String -> (String, [String])
parseUrl urlparam =
  let (url:ctrlparams) = splitUrl urlparam
  in  (url,ctrlparams)
  
--- Splits the URL parameter passed to the main script into a list of
--- strings. The strings are separated in the URL by '/'.
splitUrl :: String -> [String]
splitUrl url =
  let (ys,zs) = break (== '/') url
   in if null zs then [ys]
                 else ys : splitUrl (tail zs)

--- Gets the controller URL and the control parameters (separated by '/').
--- For instance, if the spicey script is called with the URL
--- "spicey.cgi?listEntity/arg1/arg2", this operation returns
--- ("listEntity",["arg1","arg2"]).
getControllerURL :: IO (String, [String])
getControllerURL = getUrlParameter >>= return . parseUrl

--- Gets the control parameters from the current URL.
getControllerParams :: IO [String]
getControllerParams = getUrlParameter >>= return . snd . parseUrl

--- Shows the URL corresponding to the control parameters.
--- The first argument is the URL of the controller (e.g., "listEntity")
--- and the second argument is the list of control parameters.
showControllerURL :: String -> [String] -> String
showControllerURL ctrlurl params = '?' : ctrlurl ++ concatMap ('/':) params

--------------------------------------------------------------------------
--- Standard rendering for WUI forms to edit data.
--- @param sinfo      - the UserSessionInfo to select the language
--- @param title      - the title of the WUI form
--- @param buttontag  - the text on the submit button
--- @param cancelurl  - the URL selected if submission is cancelled
--- @param envpar     - environment parameters (e.g., user session data)
--- @param hexp       - the HTML expression representing the WUI form
--- @param handler    - the handler for submitting data
renderWUI :: UserSessionInfo -> String -> String -> String
          -> a -> HtmlExp -> (HtmlEnv -> Controller) -> [HtmlExp]
renderWUI _ title buttontag cancelurl _ hexp handler =
  [h1 [htxt title],
   hexp,
   breakline,
   primSmButton buttontag (\env -> handler env >>= getPage), nbsp,
   hrefScndSmButton cancelurl [htxt "Cancel"]]


--- A WUI for manipulating CalendarTime entities.
--- It is based on a WUI for dates, i.e., the time is ignored.
wDateType :: WuiSpec ClockTime
wDateType = transformWSpec (tuple2date,date2tuple) wDate
 where
  tuple2date :: (Int, Int, Int) -> ClockTime
  tuple2date (day, month, year) =
    toClockTime (CalendarTime year month day 0 0 0 0)

  date2tuple :: ClockTime -> (Int, Int, Int)
  date2tuple ct = let CalendarTime year month day _ _ _ _ = toUTCTime ct
                  in (day, month, year)

--- A WUI for manipulating date entities.
wDate :: WuiSpec (Int, Int, Int)
wDate = wTriple (wSelectInt [1..31])
                (wSelectInt [1..12])
                (wSelectInt [1950..2050])

--- A WUI for manipulating Boolean entities. In general, this view should
--- be specialized by replacing true and false by more comprehensible strings.
wBoolean :: WuiSpec Bool
wBoolean = wSelectBool "True" "False"

--- A WUI transformer to map WUIs into WUIs for corresponding Maybe types.
wUncheckMaybe :: Eq a => a -> WuiSpec a -> WuiSpec (Maybe a)
wUncheckMaybe defval wspec =
  wMaybe (transformWSpec (not,not) (wCheckBool [htxt "No value"]))
         wspec
         defval

--- A widget for editing floating point values.
wFloat :: WuiSpec Float
wFloat = transformWSpec (readFloat, show)
            (wString `withCondition` (\s -> readMaybeFloat s /= Nothing))
 where
   readFloat s = maybe 0.0 id (readMaybeFloat s)

-- Read a float in a string.
-- Return Nothing is this is not a float string.
readMaybeFloat :: String -> Maybe Float
readMaybeFloat s =
  if all isFloatChar s
   then case reads s of
          [(x,tail)] -> if all isSpace tail then Just x else Nothing
          _          ->  Nothing
   else Nothing
 where
   isFloatChar c = isDigit c || c == '.'

--------------------------------------------------------------------------
-- Define page layout of the application.

--- The title of this application (shown in the header).
spiceyTitle :: String
spiceyTitle = "Spicey Application"

--- The home URL and brand shown at the left top of the main page.
spiceyHomeBrand :: (String, [BaseHtml])
spiceyHomeBrand = ("?", [htxt " Home"])

--- The standard footer of the Spicey page.
spiceyFooter :: [BaseHtml]
spiceyFooter =
  [par [htxt "powered by",
        href "http://www.informatik.uni-kiel.de/~pakcs/spicey"
             [image "bt4/img/spicey-logo.png" "Spicey"]
          `addAttr` ("target","_blank"),
        htxt "Framework"]]

--- Transforms a view into an HTML form by adding the basic page layout.
--- If the view is an empty text or a text starting with "?",
--- generates a redirection page.
getPage :: ViewBlock -> IO HtmlPage
getPage viewblock = case viewblock of
  [BaseText ""]          -> return $ redirectPage "spicey.cgi"
  [BaseText ('?':route)] -> return $ redirectPage ('?':route)
  _ -> do
    routemenu  <- getRouteMenu
    msg        <- getPageMessage
    login      <- getSessionLogin
    lasturl    <- getLastUrl
    withSessionCookie $ bootstrapPage favIcon cssIncludes jsIncludes
      spiceyTitle spiceyHomeBrand routemenu (rightTopMenu login)
      0 []  [h1 [htxt spiceyTitle]]
      (messageLine msg lasturl : viewblock)
      spiceyFooter
 where
  messageLine msg lasturl =
    if null msg
      then htmlStruct "header" [("class","pagemessage pagemessage-empty")]
                      [htxt ("Last page: " ++ lasturl)]
      else htmlStruct "header" [("class","pagemessage")] [htxt msg]
        
  rightTopMenu login =
    [[hrefNav "?login" (maybe [htxt "Login"]
                              (\n -> [ htxt "Logout"
                                     , htxt $ " (" ++ n ++ ")"
                                     ])
                              login)]]

favIcon :: String
favIcon = "bt4" </> "img" </> "favicon.ico"

cssIncludes :: [String]
cssIncludes =
  map (\n -> "bt4" </> "css" </> n ++ ".css")
      ["bootstrap.min", "spicey"]

jsIncludes :: [String]
jsIncludes = 
  map (\n -> "bt4" </> "js" </> n ++ ".js")
      ["jquery.slim.min", "bootstrap.bundle.min"]

-------------------------------------------------------------------------
-- Action performed when a "cancel" button is pressed.
-- In this case, a message is shown.
cancelOperation :: IO ()
cancelOperation = do
  inproc <- isInProcess
  if inproc then removeCurrentProcess else return ()
  setPageMessage $ (if inproc then "Process" else "Operation") ++ " cancelled"

-- dummy-controller to display an error
displayError :: String -> Controller
displayError msg = do
  inproc <- isInProcess
  if inproc then removeCurrentProcess else return ()
  setPageMessage ("Error occurred!" ++
                  if inproc then " Process terminated!" else "")
  if null msg
   then return [htxt "General error (shown by function Spicey.displayError)"]
   else return [htxt msg]

--- A controller to display an URL error.
displayUrlError :: Controller
displayUrlError = displayError "Illegal URL"

-- like renderTaggedTuple from WUI Library but takes list of HtmlExp
-- instead of list of strings
renderLabels :: [[HtmlExp]] -> Rendering
renderLabels labels hexps =
  spTable (map (\(l, h) -> [l, [enlargeInput h]]) (zip labels hexps))
 where
  enlargeInput h = h `addClass` "input-xxlarge"

-- Convert standard datatype values to HTML representation
stringToHtml :: HTML h => String -> h
stringToHtml s = textstyle "type_string" s

maybeStringToHtml :: HTML h => Maybe String -> h
maybeStringToHtml s = textstyle "type_string" (maybe "" id s)

intToHtml :: HTML h => Int -> h
intToHtml i = textstyle "type_int" (show i)

maybeIntToHtml :: HTML h => Maybe Int -> h
maybeIntToHtml i = textstyle "type_int" (maybe "" show i)

floatToHtml :: HTML h => Float -> h
floatToHtml i = textstyle "type_float" (show i)

maybeFloatToHtml :: HTML h => Maybe Float -> h
maybeFloatToHtml i = textstyle "type_float" (maybe "" show i)

boolToHtml :: HTML h => Bool -> h
boolToHtml b = textstyle "type_bool" (show b)

maybeBoolToHtml :: HTML h => Maybe Bool -> h
maybeBoolToHtml b = textstyle "type_bool" (maybe "" show b)

dateToHtml :: HTML h => ClockTime -> h
dateToHtml ct = textstyle "type_calendartime" (toDayString (toUTCTime ct))

maybeDateToHtml :: HTML h => Maybe ClockTime -> h
maybeDateToHtml ct =
  textstyle "type_calendartime" (maybe "" (toDayString . toUTCTime) ct)

userDefinedToHtml :: (Show a, HTML h) => a -> h
userDefinedToHtml ud = textstyle "type_string" (show ud)

maybeUserDefinedToHtml :: (Show a, HTML h) => Maybe a -> h
maybeUserDefinedToHtml ud = textstyle "type_string" (maybe "" show ud)

--------------------------------------------------------------------------
-- Auxiliary HTML items:

--- Standard table in Spicey.
spTable :: HTML h => [[[h]]] -> h
spTable items = table items  `addClass` "table table-hover table-condensed"

--------------------------------------------------------------------------
-- The page messages are implemented by a session store.
-- This store contains a message which is shown
-- in the next HTML page of a session.

--- Definition of the session state to store the page message (a string).
pageMessage :: SessionStore String
pageMessage = sessionStore "pageMessage"

--- Gets the page message and delete it.
getPageMessage :: IO String
getPageMessage = do
  msg <- fromFormReader $ getSessionData pageMessage ""
  removeSessionData pageMessage
  return msg

--- Set the page message of the current session.
setPageMessage :: String -> IO ()
setPageMessage msg = putSessionData pageMessage msg

--------------------------------------------------------------------------
-- Another example for using sessions.
-- We store the list of selected URLs into  the current session.

--- Definition of the session state to store the last URL (as a string).
lastUrls :: SessionStore [String]
lastUrls = sessionStore "lastUrls"

--- Gets the list of URLs of the current session.
getLastUrls :: IO [String]
getLastUrls = fromFormReader $ getSessionData lastUrls []

--- Gets the last URL of the current session (or "?").
getLastUrl :: IO String
getLastUrl = do urls <- getLastUrls
                return (if null urls then "?" else head urls)

--- Saves the last URL of the current session.
saveLastUrl :: String -> IO ()
saveLastUrl url = do
  urls <- getLastUrls
  putSessionData lastUrls (url:urls)

--------------------------------------------------------------------------
