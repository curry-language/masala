module Controller.CurryModule
  ( mainCurryModuleController, newCurryModuleForm, editCurryModuleForm ) where

import Data.Time
import HTML.Base
import HTML.Session
import HTML.WUI
import Masala2
import Config.EntityRoutes
import Config.UserProcesses
import System.SessionInfo
import System.Authorization
import System.AuthorizedActions
import System.Spicey
import View.EntitiesToHtml
import View.CurryModule
import Database.CDBI.Connection

type NewCurryModule = String

--- Choose the controller for a CurryModule entity according to the URL parameter.
mainCurryModuleController :: Controller
mainCurryModuleController =
  do args <- getControllerParams
     case args of
       [] -> listCurryModuleController
       ["list"] -> listCurryModuleController
       ["new"] -> newCurryModuleController
       ["show",s] -> controllerOnKey s showCurryModuleController
       ["edit",s] -> controllerOnKey s editCurryModuleController
       ["delete",s] -> controllerOnKey s deleteCurryModuleController
       ["destroy",s] -> controllerOnKey s destroyCurryModuleController
       _ -> displayUrlError

--- Shows a form to create a new CurryModule entity.
newCurryModuleController :: Controller
newCurryModuleController =
  checkAuthorization (curryModuleOperationAllowed NewEntity)
   $ (\sinfo ->
     do setParWuiStore newCurryModuleStore sinfo ""
        return [formElem newCurryModuleForm])

--- A WUI form to create a new CurryModule entity.
--- The default values for the fields are stored in 'newCurryModuleStore'.
newCurryModuleForm :: HtmlFormDef (UserSessionInfo,WuiStore NewCurryModule)
newCurryModuleForm =
  pwui2FormDef "Controller.CurryModule.newCurryModuleForm" newCurryModuleStore
   (\_ -> wCurryModule)
   (\_ entity ->
     checkAuthorization (curryModuleOperationAllowed NewEntity)
      (\_ ->
        transactionController (runT (createCurryModuleT entity))
         (nextInProcessOr (redirectController "?CurryModule/list") Nothing)))
   (\sinfo ->
     renderWUI sinfo "Create new CurryModule" "Create" "?CurryModule/list" ())

--- The data stored for executing the "new entity" WUI form.
newCurryModuleStore :: SessionStore (UserSessionInfo,WuiStore NewCurryModule)
newCurryModuleStore = sessionStore "newCurryModuleStore"

--- Transaction to persist a new CurryModule entity to the database.
createCurryModuleT :: NewCurryModule -> DBAction ()
createCurryModuleT name = newCurryModule name >>= (\_ -> return ())

--- Shows a form to edit the given CurryModule entity.
editCurryModuleController :: CurryModule -> Controller
editCurryModuleController curryModuleToEdit =
  checkAuthorization
   (curryModuleOperationAllowed (UpdateEntity curryModuleToEdit))
   $ (\sinfo ->
     do setParWuiStore editCurryModuleStore (sinfo,curryModuleToEdit)
         curryModuleToEdit
        return [formElem editCurryModuleForm])

--- A WUI form to edit a CurryModule entity.
--- The default values for the fields are stored in 'editCurryModuleStore'.
editCurryModuleForm
  :: HtmlFormDef ((UserSessionInfo,CurryModule),WuiStore CurryModule)
editCurryModuleForm =
  pwui2FormDef "Controller.CurryModule.editCurryModuleForm"
   editCurryModuleStore
   (\(_,curryModule) -> wCurryModuleType curryModule)
   (\_ entity@curryModuleToEdit ->
     checkAuthorization
      (curryModuleOperationAllowed (UpdateEntity curryModuleToEdit))
      (\_ ->
        transactionController (runT (updateCurryModuleT entity))
         (nextInProcessOr (redirectController "?CurryModule/list") Nothing)))
   (\(sinfo,_) ->
     renderWUI sinfo "Edit CurryModule" "Change" "?CurryModule/list" ())

--- The data stored for executing the edit WUI form.
editCurryModuleStore
  :: SessionStore ((UserSessionInfo,CurryModule),WuiStore CurryModule)
editCurryModuleStore = sessionStore "editCurryModuleStore"

--- Transaction to persist modifications of a given CurryModule entity
--- to the database.
updateCurryModuleT :: CurryModule -> DBAction ()
updateCurryModuleT curryModule = updateCurryModule curryModule

--- Deletes a given CurryModule entity (after asking for confirmation)
--- and proceeds with the list controller.
deleteCurryModuleController :: CurryModule -> Controller
deleteCurryModuleController curryModule =
  checkAuthorization (curryModuleOperationAllowed (DeleteEntity curryModule))
   $ (\sinfo ->
     confirmDeletionPage sinfo
      (concat
        ["Really delete entity \"",curryModuleToShortView curryModule,"\"?"]))

--- Deletes a given CurryModule entity
--- and proceeds with the list controller.
destroyCurryModuleController :: CurryModule -> Controller
destroyCurryModuleController curryModule =
  checkAuthorization (curryModuleOperationAllowed (DeleteEntity curryModule))
   $ (\_ ->
     transactionController (runT (deleteCurryModuleT curryModule))
      (redirectController "?CurryModule/list"))

--- Transaction to delete a given CurryModule entity.
deleteCurryModuleT :: CurryModule -> DBAction ()
deleteCurryModuleT curryModule = deleteCurryModule curryModule

--- Lists all CurryModule entities with buttons to show, delete,
--- or edit an entity.
listCurryModuleController :: Controller
listCurryModuleController =
  checkAuthorization (curryModuleOperationAllowed ListEntities)
   $ (\sinfo ->
     do curryModules <- runQ queryAllCurryModules
        return (listCurryModuleView sinfo curryModules))

--- Shows a CurryModule entity.
showCurryModuleController :: CurryModule -> Controller
showCurryModuleController curryModule =
  checkAuthorization (curryModuleOperationAllowed (ShowEntity curryModule))
   $ (\sinfo -> do return (showCurryModuleView sinfo curryModule))