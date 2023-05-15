module Controller.Package
  ( mainPackageController, newPackageForm, editPackageForm ) where

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
import View.Package
import Database.CDBI.Connection

type NewPackage = (String,Bool)

--- Choose the controller for a Package entity according to the URL parameter.
mainPackageController :: Controller
mainPackageController =
  do args <- getControllerParams
     case args of
       [] -> listPackageController
       ["list"] -> listPackageController
       ["new"] -> newPackageController
       ["show",s] -> controllerOnKey s showPackageController
       ["edit",s] -> controllerOnKey s editPackageController
       ["delete",s] -> controllerOnKey s deletePackageController
       ["destroy",s] -> controllerOnKey s destroyPackageController
       _ -> displayUrlError

--- Shows a form to create a new Package entity.
newPackageController :: Controller
newPackageController =
  checkAuthorization (packageOperationAllowed NewEntity)
   $ (\sinfo ->
     do setParWuiStore newPackageStore sinfo ("",False)
        return [formElem newPackageForm])

--- A WUI form to create a new Package entity.
--- The default values for the fields are stored in 'newPackageStore'.
newPackageForm :: HtmlFormDef (UserSessionInfo,WuiStore NewPackage)
newPackageForm =
  pwui2FormDef "Controller.Package.newPackageForm" newPackageStore
   (\_ -> wPackage)
   (\_ entity ->
     checkAuthorization (packageOperationAllowed NewEntity)
      (\_ ->
        transactionController (runT (createPackageT entity))
         (nextInProcessOr (redirectController "?Package/list") Nothing)))
   (\sinfo ->
     renderWUI sinfo "Create new Package" "Create" "?Package/list" ())

--- The data stored for executing the "new entity" WUI form.
newPackageStore :: SessionStore (UserSessionInfo,WuiStore NewPackage)
newPackageStore = sessionStore "newPackageStore"

--- Transaction to persist a new Package entity to the database.
createPackageT :: NewPackage -> DBAction ()
createPackageT (name,abandoned) =
  newPackage name abandoned >>= (\newentity -> return ())

--- Shows a form to edit the given Package entity.
editPackageController :: Package -> Controller
editPackageController packageToEdit =
  checkAuthorization (packageOperationAllowed (UpdateEntity packageToEdit))
   $ (\sinfo ->
     do setParWuiStore editPackageStore (sinfo,packageToEdit) packageToEdit
        return [formElem editPackageForm])

--- A WUI form to edit a Package entity.
--- The default values for the fields are stored in 'editPackageStore'.
editPackageForm :: HtmlFormDef ((UserSessionInfo,Package),WuiStore Package)
editPackageForm =
  pwui2FormDef "Controller.Package.editPackageForm" editPackageStore
   (\(_,package) -> wPackageType package)
   (\_ entity@packageToEdit ->
     checkAuthorization (packageOperationAllowed (UpdateEntity packageToEdit))
      (\_ ->
        transactionController (runT (updatePackageT entity))
         (nextInProcessOr (redirectController "?Package/list") Nothing)))
   (\(sinfo,_) -> renderWUI sinfo "Edit Package" "Change" "?Package/list" ())

--- The data stored for executing the edit WUI form.
editPackageStore :: SessionStore ((UserSessionInfo,Package),WuiStore Package)
editPackageStore = sessionStore "editPackageStore"

--- Transaction to persist modifications of a given Package entity
--- to the database.
updatePackageT :: Package -> DBAction ()
updatePackageT package = updatePackage package

--- Deletes a given Package entity (after asking for confirmation)
--- and proceeds with the list controller.
deletePackageController :: Package -> Controller
deletePackageController package =
  checkAuthorization (packageOperationAllowed (DeleteEntity package))
   $ (\sinfo ->
     confirmDeletionPage sinfo
      (concat ["Really delete entity \"",packageToShortView package,"\"?"]))

--- Deletes a given Package entity
--- and proceeds with the list controller.
destroyPackageController :: Package -> Controller
destroyPackageController package =
  checkAuthorization (packageOperationAllowed (DeleteEntity package))
   $ (\_ ->
     transactionController (runT (deletePackageT package))
      (redirectController "?Package/list"))

--- Transaction to delete a given Package entity.
deletePackageT :: Package -> DBAction ()
deletePackageT package = deletePackage package

--- Lists all Package entities with buttons to show, delete,
--- or edit an entity.
listPackageController :: Controller
listPackageController =
  checkAuthorization (packageOperationAllowed ListEntities)
   $ (\sinfo ->
     do packages <- runQ queryAllPackages
        return (listPackageView sinfo packages))

--- Shows a Package entity.
showPackageController :: Package -> Controller
showPackageController package =
  checkAuthorization (packageOperationAllowed (ShowEntity package))
   $ (\sinfo -> do return (showPackageView sinfo package))