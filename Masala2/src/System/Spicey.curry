------------------------------------------------------------------------------
--- This module implements auxiliary operations to support the
--- generic implementation of the Spicey entities.
------------------------------------------------------------------------------

module System.Spicey (
  ViewBlock, Controller, EntityController(..),
  showRoute, editRoute, deleteRoute, listRoute,
  applyControllerOn, redirectController, nextInProcessOr,
  transactionController,
  confirmDeletionPage, confirmDeletionPageWithCancelURL,
  getControllerURL,getControllerParams, showControllerURL,
  getPage, wDateType, wBoolean, wUncheckMaybe, wFloat,
  displayError, displayUrlError, cancelOperation,
  renderWUI, renderWUIWithText, renderLabels,
  stringToHtml, maybeStringToHtml,
  intToHtml,maybeIntToHtml, floatToHtml, maybeFloatToHtml,
  boolToHtml, maybeBoolToHtml, dateToHtml, maybeDateToHtml, timeToHtml,
  userDefinedToHtml, maybeUserDefinedToHtml,
  spTable, smallMutedText,
  setPageMessage, getPageMessage,
  saveLastUrl, getLastUrl, getLastUrls
  ) where

import Data.Char           ( isSpace, isDigit, toLower )
import Data.List           ( intersperse, nub, split )

import Data.Time
import System.FilePath     ( (</>) )

import Database.CDBI.Connection ( SQLResult )
import HTML.Base
import HTML.Session
import HTML.Styles.Bootstrap4
import HTML.WUI

import Config.Roles
import Config.UserProcesses
import System.Routes
import System.Processes
import System.Authentication

--------------------------------------------------------------------------
--- A `ViewBlock` can be turned into a representation which can be displayed
--- in a web page. Thus, it is a basic HTML document.
--- The transformation of a `ViewBlock` into the displayed web page is done
--- by the operation 'getPage'.
type ViewBlock = [BaseHtml]

--- Controllers contains all logic and their result should be a `ViewBlock`.
--- if the behavior of controller should depend on URL parameters
--- (following the first name specifying the controller), one
--- can access these URL parameters by using the operation
--- Spicey.getControllerParams inside the controller.
type Controller = IO ViewBlock

------------------------------------------------------------------------------
-- Auxiliaries for routing.

--- The type class `EntityController` contains:
--- * the application of a controller to some entity identified by a key string
--- * an operation to construct a URL route for an entity w.r.t. to a route
---   string
--- The instances for all entitiy types are defined in the module
--- `Config.EntityRoutes`.
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

--- Returns the URL route to list all entities of the type of the given entity.
--- The given entity is not evaluated but only used to resolve the
--- overloaded type instance.
listRoute :: EntityController a => a -> String
listRoute a = let xs = split (=='/') (showRoute a)
              in case xs of []  -> "?" -- should not occur
                            x:_ -> x ++ "/list"

------------------------------------------------------------------------------
-- Auxiliaries for controllers.

--- Reads an entity for a given key and applies a controller to it.
applyControllerOn :: Maybe enkey -> (enkey -> IO en)
                  -> (en -> Controller) -> Controller
applyControllerOn Nothing        _       _              = displayUrlError
applyControllerOn (Just userkey) getuser usercontroller =
  getuser userkey >>= usercontroller

--- A controller to redirect to an URL starting with "?"
--- (see implementation of `getPage`).
redirectController :: String -> Controller
redirectController url = return [htmlText url]

--- A controller to execute a transaction and proceed, if the transaction
--- succeeds, with a given controller applied to the transaction result.
--- Otherwise, the transaction error is shown.
--- @param trans - the transaction to be executed
--- @param controller - the controller executed in case of success
transactionController :: IO (SQLResult a) -> (a -> Controller) -> Controller
transactionController trans controller =
  trans >>= either (displayError . show) controller

--- If we are in a process, execute the next process depending on
--- the provided information passed in the second argument,
--- otherwise execute the given controller (first argument).
nextInProcessOr :: Controller -> Maybe ControllerResult -> Controller
nextInProcessOr controller arg = do
  isproc <- isInProcess
  if isproc then advanceInProcess arg >> return [htxt ""] -- triggers redirect
            else controller

------------------------------------------------------------------------------
--- Generates a page to ask the user for a confirmation to delete an entity
--- specified in the controller URL (of the form "entity/delete/key/...").
--- The yes/no answers are references derived from the controller URL
--- where the second argument is replaced by "destroy"/"show".
--- @param sinfo - information about the current user session
--- @param question - a question asked
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


confirmDeletionPageWithCancelURL :: UserSessionInfo -> String -> String
                                 -> Controller
confirmDeletionPageWithCancelURL _ question cancelurl = do
  (entity,ctrlargs) <- getControllerURL
  case ctrlargs of
    (_:args) -> return $
      [h3 [htxt question],
       par [hrefPrimSmButton (showControllerURL entity ("destroy":args))
                             [htxt "Yes"],
            nbsp,
            hrefScndSmButton cancelurl [htxt "No"]]]
    _ -> displayUrlError


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

--- Standard rendering for WUI forms to edit data.
--- @param sinfo      - the UserSessionInfo to select the language
--- @param title      - the title of the WUI form
--- @param buttontag  - the text on the submit button
--- @param cancelurl  - the URL selected if submission is cancelled
--- @param envpar     - environment parameters (e.g., user session data)
--- @param hexp       - the HTML expression representing the WUI form
--- @param handler    - the handler for submitting data
renderWUIWithText :: UserSessionInfo -> String -> String -> [HtmlExp]
  -> String -> HtmlExp -> (HtmlEnv -> Controller) -> [HtmlExp]
renderWUIWithText _ title buttontag cmts cancelurl hexp handler =
  [h1 [htxt title]] ++ cmts ++
  [hexp,
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
spiceyTitle = "Masala: The Repository of Curry Packages (TEST VERSION!)"

--- The home URL and brand shown at the left top of the main page.
spiceyHomeBrand :: (String, [BaseHtml])
spiceyHomeBrand = ("?", [htxt " Masala"])

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
    withSessionCookie $ bootstrapPage2 favIcon cssIncludes jsIncludes
      spiceyTitle spiceyHomeBrand
      (addNavItemClass routemenu) (rightTopMenu login)
      0 []
      [h1 [htxt "Masala: ",
           smallMutedText "The Repository of Curry Packages (TEST VERSION!)"]]
      (messageLine msg lasturl : viewblock)
      spiceyFooter
 where
  addNavItemClass = map (\i -> ("nav-item", i))

  messageLine msg lasturl =
    if null msg
      then htmlStruct "header" [("class","pagemessage pagemessage-empty")]
                      [htxt ("Last page: " ++ lasturl)]
      else htmlStruct "header" [("class","pagemessage")] [htxt msg]
        
  rightTopMenu login =
    maybe [ddRegMenu]
          (\ (n,r) -> (if r == roleAdmin then [ddAdminMenu] else []) ++
                      [ddUserMenu n] )
          login
   where
    ddRegMenu = dropDownMenu [htxt "Login/Register", dropDownIcon]
                 (map (\ (hr,he) -> href hr he `addClass` "dropdown-item")
                      [ ("?login",        [htxt "Login"])
                      , ("?Registration", [htxt "New registration"])
                      , ("?Validation",   [htxt "Validate your account"])
                      ])

    ddUserMenu n =
      dropDownMenu [userWhiteIcon, htxt $ " " ++ n, dropDownIcon]
                   (map (\ (hr,he) -> href hr he `addClass` "dropdown-item")
                        userMenu)

    ddAdminMenu = dropDownMenu [htxt "Administrator", dropDownIcon]
        (map (\ (hr,he) -> href hr he `addClass` "dropdown-item")
             [ ("?User/list", [htxt "All users"])
             , ("?ValidationToken/list", [htxt "Validation tokens"])
             , ("?Version/unpublished", [htxt "Unpublished package versions"])
             ])

-- A dropdown menu (represented as a HTML list item).
-- The first argument is the title (as HTML expressions) and
-- the second argument is the actual menu (a list of elements with
-- class "dropdown-item").
dropDownMenu :: [BaseHtml] -> [BaseHtml] -> (String,[BaseHtml])
dropDownMenu title ddmenu =
  ("nav-item dropdown",
   [hrefNav "#" title
    `addAttrs` [("class","dropdown-toggle"),
                ("id", "dropdownuser"),
                ("data-toggle","dropdown"),
                ("aria-haspopup", "true"),
                ("aria-expanded", "false")],
   blockstyle "dropdown-menu dropdown-menu-right" ddmenu
     `addAttr` ("area-labelledby", "dropdownuser")])

--- The menu for a user if it he is logged in.
userMenu :: [(String,[BaseHtml])]
userMenu =
  [ ("?User/profile",     [htxt "Show profile"])
  , ("?User/editprofile", [htxt "Change profile"])
  , ("?User/password",    [htxt "Change password"])
  , ("?User/maintaining", [htxt "Maintained packages"])
  , ("?User/watching",    [htxt "Watched packages"])
  , ("?Mail/admin",       [htxt "Send mail to admin"])
  , ("?login",            [htxt "Logout"])
  ]

favIcon :: String
favIcon = "bt4" </> "img" </> "favicon.ico"

cssIncludes :: [String]
cssIncludes =
  map (\n -> "bt4" </> "css" </> n ++ ".css")
      ["bootstrap.min", "spicey"]

jsIncludes :: [String]
jsIncludes = 
  map (\n -> "bt4" </> "js" </> n ++ ".js")
      ["jquery.slim.min", "bootstrap.bundle.min", "spicey"]

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

timeToHtml :: HTML h => ClockTime -> h
timeToHtml = textstyle "type_calendartime"  . calendarTimeToString . toUTCTime

userDefinedToHtml :: (Show a, HTML h) => a -> h
userDefinedToHtml ud = textstyle "type_string" (show ud)

maybeUserDefinedToHtml :: (Show a, HTML h) => Maybe a -> h
maybeUserDefinedToHtml ud = textstyle "type_string" (maybe "" show ud)

--------------------------------------------------------------------------
-- Auxiliary HTML items:

--- Standard table in Spicey.
spTable :: HTML h => [[[h]]] -> h
spTable items = table items  `addClass` "table table-hover table-condensed"

--- A small muted text (used in the title):
smallMutedText :: HTML h => String -> h
smallMutedText s = htmlStruct "small" [("class","text-muted")] [htxt s]

--------------------------------------------------------------------------
-- Icons:

--- User (white) icon:
userWhiteIcon :: HTML h => h
userWhiteIcon =
  image "bt4/img/user-white.svg" "User"
    `addAttrs` [("width","16"), ("height","16")]

--- Drowdown icon:
dropDownIcon :: HTML h => h
dropDownIcon = image "bt4/img/caret-down-white.svg" "Open"

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
